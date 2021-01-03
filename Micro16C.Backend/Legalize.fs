module Micro16C.Backend.Legalize

open Micro16C.MiddleEnd.IR

let legalizeConstants (irModule: Module): Module =

    let checkOps instr =

        // "and" instruction where one side of the value is 0x8000 whose only User is a "br" instruction are exempt
        // from this pass as these are later lowered to a conditional branch on the N flag
        match !instr with
        | { Content = BinaryInstruction { Kind = And
                                          Left = Ref { Content = Constant { Value = 0x8000s } } }
            Users = [ Ref { Content = CondBrInstruction _ } ] }
        | { Content = BinaryInstruction { Kind = And
                                          Right = Ref { Content = Constant { Value = 0x8000s } } }
            Users = [ Ref { Content = CondBrInstruction _ } ] } -> ()
        | _ ->
            !instr
            |> Value.operands
            |> List.indexed
            |> List.choose (fun (i, x) ->
                match !x with
                | { Content = Constant { Value = c } } when c <> 0s && c <> 1s && c <> -1s -> Some(i, c)
                | _ -> None)
            |> List.iter (fun (i, c) ->

                // If the constant is an operand of the phi we can't place it in front of the phi; phis need to be
                // at the start of a basic block before any non phi instructions. Instead we'll check the predecessor
                // where the constant comes from and put the instructions before it's terminating instruction
                let builder =
                    match !instr with
                    | { Content = PhiInstruction { Incoming = _ } } ->
                        let pred =
                            !instr |> Value.operands |> List.item (i + 1)

                        Builder.Default
                        |> Builder.setInsertBlock (Some pred)
                        |> Builder.setInsertPoint
                            ((!pred)
                             |> Value.asBasicBlock
                             |> BasicBlock.terminator
                             |> Before)
                    | _ ->
                        Builder.Default
                        |> Builder.setInsertBlock ((!instr).ParentBlock)
                        |> Builder.setInsertPoint (Before instr)

                let bitPairs =
                    Seq.unfold (fun c ->
                        if c = 0s then
                            None
                        else
                            let lower2 = c &&& 0b11s
                            // Logical right shift needed, not an arithmetic shift
                            Some(lower2, ((c |> uint16) >>> 2) |> int16)) c
                    |> Seq.rev

                // If the number were to consist of an uneven amount of active bits, then the very first value in the
                // sequence is 1. The fold below does not account for bitPairs that are actually not pairs but a single
                // bit so we need to set the start value to 1 instead of 0 instead
                let (start, bitPairs) =
                    match Seq.head bitPairs with
                    | 0b01s -> (1s, Seq.tail bitPairs)
                    | _ -> (0s, bitPairs)

                let c =
                    bitPairs
                    |> Seq.fold (fun (op, builder) bitPair ->
                        match bitPair with
                        | 0b00s ->
                            builder
                            |> Builder.createBinary op Add op
                            ||> Builder.createUnary Shl
                        | 0b01s ->
                            (op, builder)
                            ||> Builder.createUnary Shl
                            ||> Builder.createUnary Shl
                            ||> Builder.createBinary (Builder.createConstant 1s) Add
                        | 0b10s ->
                            (op, builder)
                            ||> Builder.createUnary Shl
                            ||> Builder.createBinary (Builder.createConstant 1s) Add
                            ||> Builder.createUnary Shl
                        | 0b11s ->
                            (op, builder)
                            ||> Builder.createBinary (Builder.createConstant 1s) Add
                            ||> Builder.createUnary Shl
                            ||> Builder.createBinary (Builder.createConstant 1s) Add
                            ||> Builder.createUnary Shl
                        | _ -> failwithf "Internal Compiler Error: Invalid bit pair %d" bitPair)
                           (Builder.createConstant start, builder)
                    |> fst

                instr |> Value.setOperand i c)

            ()

    irModule
    |> Module.instructions
    |> List.iter checkOps

    irModule

let genPhiMoves (irModule: Module): Module =

    let builder = Builder.fromModule irModule

    let builder =
        irModule
        |> Module.basicBlocks
        |> List.map (fun x -> (x, !x |> BasicBlock.predecessors))
        |> List.fold (fun builder (b, preds) ->
            preds
            |> List.fold (fun builder p ->
                let n, builder =
                    if !b |> BasicBlock.predecessors |> List.length > 1
                       && !p |> BasicBlock.successors |> List.length > 1 then
                        let block, builder =
                            builder |> Builder.createBasicBlockAt (After p) ""

                        !p
                        |> Value.asBasicBlock
                        |> BasicBlock.terminator
                        |> Value.replaceOperand b block

                        (block,
                         builder
                         |> Builder.setInsertBlock (Some block)
                         |> Builder.createGoto b
                         |> snd)
                    else
                        (p, builder)

                !b
                |> Value.asBasicBlock
                |> BasicBlock.phis
                |> List.fold (fun builder phi ->
                    let blockIndex =
                        !phi
                        |> Value.operands
                        |> List.indexed
                        |> List.findIndex (snd >> (=) p)

                    let operand =
                        !phi
                        |> Value.operands
                        |> List.item (blockIndex - 1)

                    let move, builder =
                        builder
                        |> Builder.setInsertBlock (Some n)
                        |> Builder.setInsertPoint
                            (!n
                             |> Value.asBasicBlock
                             |> BasicBlock.terminator
                             |> Before)
                        |> Builder.createMove operand

                    phi |> Value.setOperand (blockIndex - 1) move
                    phi |> Value.setOperand blockIndex n
                    builder) builder) builder) builder

    Builder.finalize builder
