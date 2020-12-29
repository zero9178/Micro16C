module Micro16C.MiddleEnd.Passes

open Micro16C.MiddleEnd.IR


let instructionSimplify (irModule: Module) =
    let simplify value =
        match !value with
        | { Content = BinaryInstruction { Kind = And
                                          Left = Ref { Content = Constant { Value = 0s } }
                                          Right = _ } }
        | { Content = BinaryInstruction { Kind = And
                                          Right = Ref { Content = Constant { Value = 0s } }
                                          Left = _ } } ->
            value
            |> Value.replaceWith (Builder.createConstant 0s)
        | { Content = BinaryInstruction { Kind = Add
                                          Left = Ref { Content = Constant { Value = 0s } }
                                          Right = passThrough } }
        | { Content = BinaryInstruction { Kind = Add
                                          Right = Ref { Content = Constant { Value = 0s } }
                                          Left = passThrough } } -> value |> Value.replaceWith passThrough
        | { Content = BinaryInstruction { Kind = And
                                          Left = Ref { Content = Constant { Value = 0xFFFFs } }
                                          Right = passThrough } }
        | { Content = BinaryInstruction { Kind = And
                                          Right = Ref { Content = Constant { Value = 0xFFFFs } }
                                          Left = passThrough } } -> value |> Value.replaceWith passThrough
        | { Content = BinaryInstruction { Kind = Add
                                          Left = Ref { Content = Constant { Value = lhs } }
                                          Right = Ref { Content = Constant { Value = rhs } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs + rhs))
        | { Content = BinaryInstruction { Kind = And
                                          Left = Ref { Content = Constant { Value = lhs } }
                                          Right = Ref { Content = Constant { Value = rhs } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs &&& rhs))
        | { Content = BinaryInstruction { Kind = And; Left = lhs; Right = rhs } } when (!lhs).Name = (!rhs).Name ->
            value |> Value.replaceWith lhs
        | { Content = UnaryInstruction { Kind = Not
                                         Value = Ref { Content = Constant { Value = rhs } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant (~~~rhs))
        | { Content = UnaryInstruction { Kind = Shl
                                         Value = Ref { Content = Constant { Value = constant } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant (constant <<< 1))
        | { Content = UnaryInstruction { Kind = Shr
                                         Value = Ref { Content = Constant { Value = constant } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant ((constant |> uint16) >>> 1 |> int16))
        | _ -> ()

    irModule
    |> Module.instructions
    |> Seq.iter simplify

    irModule

let deadCodeElimination (irModule: Module) =

    let eliminate value =
        if not (Value.hasSideEffects !value)
           && 0 = Value.useCount !value then
            value |> Value.eraseFromParent

    irModule
    |> Module.instructions
    |> Seq.iter eliminate

    irModule

let simplifyCFG (irModule: Module) =

    let simplifyBlock blockValue =
        match !blockValue with
        | { Content = BasicBlockValue block } ->
            // this optimization may be invalid if the basic block is used in a Phi. For now I'll be conservative and
            // not remove such basic blocks. As a future TODO I could check for semantic changes
            match block.Instructions with
            | [ Ref { Content = GotoInstruction { BasicBlock = destination } } ] when not
                                                                                          (List.exists (fun x ->
                                                                                              match !x with
                                                                                              | { Content = PhiInstruction _ } ->
                                                                                                  true
                                                                                              | _ -> false)
                                                                                               (!blockValue).Users) ->
                blockValue |> Value.replaceWith destination
                false
            | _ -> true
        | _ -> failwith "Internal Compiler Error"

    let blocks =
        irModule
        |> Module.basicBlocks
        |> Seq.filter simplifyBlock
        |> List.ofSeq

    { BasicBlocks = blocks }

let instructionCombine (irModule: Module) =
    let combineInBlock blockValue =

        let rec combine (instructions: Value ref list) =
            match instructions with
            | Ref { Content = UnaryInstruction { Kind = Not
                                                 Value = Ref { Content = UnaryInstruction { Kind = Not
                                                                                            Value = passThrough }
                                                               Users = [ _ ] } as first } } as second :: tail ->
                second |> Value.replaceWith passThrough
                first |> Value.eraseFromParent
                combine tail
            | _ :: tail -> combine tail
            | [] -> ()


        let block = !blockValue |> Value.asBasicBlock
        combine block.Instructions

    irModule
    |> Module.basicBlocks
    |> List.iter combineInBlock

    irModule

let analyzeAlloc (irModule: Module) =
    let analyzeAlloc instr =
        match !instr with
        | { Content = AllocationInstruction ({ Aliased = None } as alloca)
            Users = users } ->
            let addressTaken =
                users
                |> List.exists (fun x ->
                    match !x with
                    | { Content = LoadInstruction _ }
                    | { Content = StoreInstruction _ } -> false
                    | _ -> true)

            instr
            := { !instr with
                     Content =
                         AllocationInstruction
                             { alloca with
                                   Aliased = Some addressTaken } }
        | _ -> ()

    irModule
    |> Module.instructions
    |> Seq.iter analyzeAlloc

    irModule

let removeRedundantLoadStores (irModule: Module) =
    let removeRedundantLoadStoresInBlock blockValue =
        let block = !blockValue |> Value.asBasicBlock

        let simplifyLoadStoreSeries instr =
            // replace all loads first therefore making all stores but the last redundant
            instr
            |> List.fold (fun replacement x ->
                match !x with
                | { Content = StoreInstruction { Value = passThrough } } -> passThrough |> Some
                | { Content = LoadInstruction _ } ->
                    match replacement with
                    | Some value ->
                        x |> Value.replaceWith value
                        replacement
                    | None -> Some x
                | _ -> failwith "Internal Compiler Error") None
            |> ignore

            let safeTail list =
                match list with
                | _ :: tail -> tail
                | _ -> []

            // Remove all but the last store
            instr
            |> List.filter (fun x ->
                match !x with
                | { Content = StoreInstruction _ } -> true
                | _ -> false)
            |> List.rev
            |> safeTail
            |> List.iter Value.eraseFromParent

        block.Instructions
        |> List.filter (fun x ->
            match !x with
            | { Content = LoadInstruction { Source = Ref { Content = AllocationInstruction { Aliased = Some false } } } }
            | { Content = StoreInstruction { Destination = Ref { Content = AllocationInstruction { Aliased = Some false } } } } ->
                true
            | _ -> false)
        |> List.groupBy (fun x ->
            match !x with
            | { Content = LoadInstruction { Source = alloca } }
            | { Content = StoreInstruction { Destination = alloca } } -> (!alloca).Name
            | _ -> failwith "Internal Compiler error")
        |> List.map (fun (_, x) ->
            match !(List.head x) with
            | { Content = LoadInstruction { Source = alloca } }
            | { Content = StoreInstruction { Destination = alloca } } -> (alloca, x)
            | _ -> failwith "Internal Compiler error")
        |> List.iter (snd >> simplifyLoadStoreSeries)



    irModule
    |> Module.basicBlocks
    |> List.iter removeRedundantLoadStoresInBlock

    irModule
