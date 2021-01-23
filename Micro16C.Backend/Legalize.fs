module Micro16C.Backend.Legalize

open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util

let legalizeConstants irModule =

    let builder = Builder.fromModule irModule

    let checkOps instr =
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

                    builder
                    |> Builder.setInsertBlock (Some pred)
                    |> Builder.setInsertPoint
                        ((!pred)
                         |> Value.asBasicBlock
                         |> BasicBlock.terminator
                         |> Before)
                | _ ->
                    builder
                    |> Builder.setInsertBlock (!instr |> Value.parentBlock)
                    |> Builder.setInsertPoint (Before instr)

            let allBits =
                Seq.unfold (fun c -> Some(c &&& 0x8000us <> 0us, c <<< 1)) (c |> uint16)
                |> Seq.take 16

            let splitIndex =
                1
                + (allBits
                   |> Seq.windowed 2
                   |> Seq.findIndex (fun l -> l.[0] <> l.[1]))

            let (first, second) =
                allBits |> List.ofSeq |> List.splitAt splitIndex

            if (List.forall id first
                && List.forall (id >> not) second)
               || (List.forall (id >> not) first
                   && List.forall id second) then
                let pattern =
                    Seq.unfold (fun c ->
                        let c =
                            Builder.createBinary c LShr (Builder.createConstant 1s) builder
                            |> fst

                        Some(c, c)) (Builder.createConstant -1s)
                    |> Seq.take (List.length first)
                    |> Seq.last

                if not first.[0] then
                    instr |> Value.setOperand i pattern
                else
                    instr
                    |> Value.setOperand i (Builder.createUnary Not pattern builder |> fst)
            else
                let hasMore1s =
                    allBits
                    |> Seq.fold (fun res b -> if b then res + 1 else res) 0 > 8

                let c = if hasMore1s then ~~~c else c

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

                let shiftLeft value builder =
                    Builder.createBinary value Shl (Builder.createConstant 1s) builder

                let c =
                    bitPairs
                    |> Seq.fold (fun (op, builder) bitPair ->
                        match bitPair with
                        | 0b00s ->
                            match !op with
                            | { Content = Constant { Value = 0s } } -> (op, builder)
                            | _ ->
                                builder
                                |> Builder.createBinary op Add op
                                ||> shiftLeft
                        | 0b01s ->
                            match !op with
                            | { Content = Constant { Value = 0s } } -> (Builder.createConstant 1s, builder)
                            | _ ->
                                (op, builder)
                                ||> shiftLeft
                                ||> shiftLeft
                                ||> Builder.createBinary (Builder.createConstant 1s) Add
                        | 0b10s ->
                            match !op with
                            | { Content = Constant { Value = 0s } } ->
                                (Builder.createConstant 1s, builder) ||> shiftLeft
                            | _ ->
                                (op, builder)
                                ||> shiftLeft
                                ||> Builder.createBinary (Builder.createConstant 1s) Add
                                ||> shiftLeft
                        | 0b11s ->
                            match !op with
                            | { Content = Constant { Value = 0s } } ->
                                (Builder.createConstant 1s, builder)
                                ||> shiftLeft
                                ||> Builder.createBinary (Builder.createConstant 1s) Add
                            | _ ->
                                (op, builder)
                                ||> shiftLeft
                                ||> Builder.createBinary (Builder.createConstant 1s) Add
                                ||> shiftLeft
                                ||> Builder.createBinary (Builder.createConstant 1s) Add
                        | _ -> failwithf "Internal Compiler Error: Invalid bit pair %d" bitPair)
                           (Builder.createConstant start, builder)
                    |> fst

                let c =
                    if hasMore1s then Builder.createUnary Not c builder |> fst else c

                instr |> Value.setOperand i c)

    !irModule
    |> Module.instructions
    |> List.iter checkOps

    irModule

let breakPhiCriticalEdges irModule =

    !irModule
    |> Module.basicBlocks
    |> List.filter
        ((!)
         >> Value.asBasicBlock
         >> BasicBlock.phis
         >> List.length
         >> (<>) 0)
    |> List.map (associateWith ((!) >> BasicBlock.predecessors))
    |> List.iter (fun (currBlock, preds) ->
        preds
        |> List.iter (fun pred ->
            if preds |> List.length > 1
               && !pred |> BasicBlock.successors |> List.length > 1 then
                // Destroy critical edges by inserting a block in between, in which we'll be able to place our
                // copy operations

                let builder = Builder.fromModule irModule

                let block, builder =
                    builder
                    |> Builder.createBasicBlockAt (After pred) ""

                let basicBlock = !block |> Value.asBasicBlock

                block
                := { !block with
                         Content =
                             BasicBlockValue
                                 { basicBlock with
                                       ImmediateDominator = Some pred } }

                !pred
                |> Value.asBasicBlock
                |> BasicBlock.terminator
                |> Value.replaceOperand currBlock block

                !currBlock
                |> Value.asBasicBlock
                |> BasicBlock.phis
                |> List.iter (Value.replaceOperand pred block)

                builder
                |> Builder.setInsertBlock (Some block)
                |> Builder.createGoto currBlock
                |> ignore))

    irModule

let legalizeInstructions irModule =

    let builder = Builder.fromModule irModule

    !irModule
    |> Module.instructions
    |> Seq.iter (fun instr ->
        match instr with
        | BinOp Sub (lhs, rhs) ->

            let replacement =
                builder
                |> Builder.setInsertBlock (!instr |> Value.parentBlock)
                |> Builder.setInsertPoint (Before instr)
                |> Builder.createUnary Not rhs
                ||> Builder.createBinary (Builder.createConstant 1s) Add
                ||> Builder.createBinary lhs Add
                |> fst

            instr |> Value.replaceWith replacement
        | BinOp Or (lhs, rhs) ->

            // R0 | R1 = ~(~R0 & ~R1)

            let builder =
                builder
                |> Builder.setInsertBlock (!instr |> Value.parentBlock)
                |> Builder.setInsertPoint (Before instr)

            let lhs =
                builder |> Builder.createUnary Not lhs |> fst

            let replacement =
                builder
                |> Builder.createUnary Not rhs
                ||> Builder.createBinary lhs And
                |> fst

            instr |> Value.replaceWith replacement
        | BinOp Xor (lhs, rhs) ->

            // R0 ^ R1 = (R1 & ~R2) + (~R1 & R2)

            let builder =
                builder
                |> Builder.setInsertBlock (!instr |> Value.parentBlock)
                |> Builder.setInsertPoint (Before instr)

            let lhsNew =
                (rhs, builder)
                ||> Builder.createUnary Not
                ||> Builder.createBinary lhs And
                |> fst

            let rhsNew =
                (lhs, builder)
                ||> Builder.createUnary Not
                ||> Builder.createBinary rhs And
                |> fst

            let replacement =
                builder
                |> Builder.createBinary lhsNew Add rhsNew
                |> fst

            instr |> Value.replaceWith replacement
        | BinOp SRem (lhs, ConstOp c)
        | BinOp URem (lhs, ConstOp c) when (c &&& (c - 1s)) = 0s ->
            // lhs mod power of two can be lowered to a simple and
            let builder = builder |> Builder.setInsertBlock None

            instr
            |> Value.replaceWith
                (builder
                 |> Builder.createBinary lhs And (Builder.createConstant (c - 1s))
                 |> fst)
        | BinOp SRem (lhs, rhs)
        | BinOp URem (lhs, rhs) ->

            let kind =
                match instr with
                | BinOp SRem _ -> SRem
                | BinOp URem _ -> URem
                | _ -> failwith "Internal Compiler Error"

            let beforeBlock, instrBlock, builder =
                builder |> Builder.splitBlockAt (Before instr)

            let cont, lhs, builder =
                if kind = SRem then
                    let neg, builder =
                        builder
                        |> Builder.createBasicBlockAt (After beforeBlock) "modNeg"

                    let cont, builder =
                        builder
                        |> Builder.createBasicBlockAt (After neg) "cont"

                    let builder = builder |> Builder.setInsertBlock None

                    !beforeBlock
                    |> Value.asBasicBlock
                    |> BasicBlock.terminator
                    |> Value.replaceWith
                        (builder
                         |> Builder.createCondBr Negative lhs neg cont
                         |> fst)

                    let negated, builder =
                        builder
                        |> Builder.setInsertBlock (Some neg)
                        |> Builder.createUnary Not lhs
                        ||> Builder.createBinary (Builder.createConstant 1s) Add


                    let phi, builder =
                        builder
                        |> Builder.createGoto cont
                        |> snd
                        |> Builder.setInsertBlock (Some cont)
                        |> Builder.createPhi [ (negated, neg)
                                               (lhs, beforeBlock) ]

                    (cont, phi, builder)
                else
                    let cont, builder =
                        builder
                        |> Builder.createBasicBlockAt (After beforeBlock) "cont"

                    let builder =
                        builder |> Builder.setInsertBlock (Some cont)

                    !beforeBlock
                    |> Value.asBasicBlock
                    |> BasicBlock.terminator
                    |> Value.replaceOperand instrBlock cont

                    (cont, lhs, builder)

            let rhs =
                builder
                |> Builder.createUnary Not rhs
                ||> Builder.createBinary (Builder.createConstant 1s) Add
                |> fst

            let body, builder =
                builder
                |> Builder.createBasicBlockAt (After cont) "modBody"

            let modCont, builder =
                builder
                |> Builder.createBasicBlockAt (After body) "modCont"

            let acc, builder =
                builder
                |> Builder.createGoto body
                |> snd
                |> Builder.setInsertBlock (Some body)
                |> Builder.createPhi [ (lhs, cont)
                                       (Value.UndefValue, modCont) ]

            let nextValue =
                builder |> Builder.createBinary acc Add rhs |> fst

            acc |> Value.setOperand 2 nextValue

            builder
            |> Builder.createCondBr Negative nextValue instrBlock modCont
            |> snd
            |> Builder.setInsertBlock (Some modCont)
            |> Builder.createGoto body
            |> ignore

            instr |> Value.replaceWith acc
        | BinOp Shl (_, ConstOp 1s) -> ()
        | BinOp LShr (_, ConstOp 1s) -> ()
        | BinOp LShr (lhs, ConstOp c)
        | BinOp Shl (lhs, ConstOp c) ->

            let kind =
                match instr with
                | BinOp Shl _ -> Shl
                | BinOp LShr _ -> LShr
                | _ -> failwith "Internal Compiler Error"

            let builder =
                builder
                |> Builder.setInsertBlock (!instr |> Value.parentBlock)
                |> Builder.setInsertPoint (Before instr)

            let repl =
                [ 1s .. c ]
                |> Seq.fold (fun current _ ->
                    builder
                    |> Builder.createBinary current kind (Builder.createConstant 1s)
                    |> fst) lhs

            instr |> Value.replaceWith repl
        | BinOp LShr (lhs, rhs)
        | BinOp Shl (lhs, rhs) ->

            let kind =
                match instr with
                | BinOp Shl _ -> Shl
                | BinOp LShr _ -> LShr
                | _ -> failwith "Internal Compiler Error"

            let beforeBlock, instrBlock, builder =
                builder |> Builder.splitBlockAt (Before instr)

            let builder =
                builder
                |> Builder.setInsertBlock (Some beforeBlock)
                |> Builder.setInsertPoint
                    (Before
                        (!beforeBlock
                         |> Value.asBasicBlock
                         |> BasicBlock.terminator))

            let negRhs =
                (rhs, builder)
                ||> Builder.createUnary Not
                ||> Builder.createBinary (Builder.createConstant 1s) Add
                |> fst

            let cond, builder =
                builder
                |> Builder.createBasicBlockAt (After beforeBlock) "shCond"

            let body, builder =
                builder
                |> Builder.createBasicBlockAt (After cond) "shBody"

            !beforeBlock
            |> Value.asBasicBlock
            |> BasicBlock.terminator
            |> Value.replaceOperand instrBlock cond

            let builder =
                builder |> Builder.setInsertBlock (Some cond)

            let i =
                builder
                |> Builder.createPhi [ (Builder.createConstant 0s, beforeBlock)
                                       (Value.UndefValue, body) ]
                |> fst

            let acc =
                builder
                |> Builder.createPhi [ (lhs, beforeBlock)
                                       (Value.UndefValue, body) ]
                |> fst

            let diff =
                builder
                |> Builder.createBinary i Add negRhs
                |> fst

            builder
            |> Builder.createCondBr Negative diff body instrBlock
            |> ignore

            let builder =
                builder |> Builder.setInsertBlock (Some body)

            let newAcc =
                builder
                |> Builder.createBinary acc kind (Builder.createConstant 1s)
                |> fst

            acc |> Value.setOperand 2 newAcc

            let newI =
                builder
                |> Builder.createBinary i Add (Builder.createConstant 1s)
                |> fst

            i |> Value.setOperand 2 newI

            builder |> Builder.createGoto cond |> ignore
            instr |> Value.replaceWith acc
        | _ -> ())

    irModule
