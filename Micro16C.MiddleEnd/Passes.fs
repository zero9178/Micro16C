module Micro16C.MiddleEnd.Passes

open System.Collections.Generic
open System.Collections.Immutable
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
        | { Content = BinaryInstruction { Kind = And; Left = lhs; Right = rhs } } when lhs = rhs ->
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
        | { Content = CondBrInstruction { Kind = NotZero
                                          Value = Ref { Content = Constant { Value = constant } }
                                          TrueBranch = trueBranch
                                          FalseBranch = falseBranch } } ->
            if constant <> 0s then
                value
                |> Value.replaceWith
                    (Builder.createGoto trueBranch Builder.Default
                     |> fst)
            else
                value
                |> Value.replaceWith
                    (Builder.createGoto falseBranch Builder.Default
                     |> fst)
        | { Content = CondBrInstruction { Kind = Negative
                                          Value = Ref { Content = Constant { Value = constant } }
                                          TrueBranch = trueBranch
                                          FalseBranch = falseBranch } } ->
            if constant < 0s then
                value
                |> Value.replaceWith
                    (Builder.createGoto trueBranch Builder.Default
                     |> fst)
            else
                value
                |> Value.replaceWith
                    (Builder.createGoto falseBranch Builder.Default
                     |> fst)
        | { Content = PhiInstruction { Register = None; Incoming = list } } ->
            let distinctValues = list |> List.map fst |> List.distinct

            if distinctValues |> List.length = 1 then
                value
                |> Value.replaceWith (List.head distinctValues)
        | _ -> ()

    irModule
    |> Module.instructions
    |> List.iter simplify

    irModule

let deadCodeElimination (irModule: Module) =

    let eliminate value =
        if not (Value.hasSideEffects !value)
           && 0 = Value.useCount !value then
            value |> Value.eraseFromParent

    irModule
    |> Module.instructions
    |> List.rev
    |> List.iter eliminate

    irModule

let simplifyCFG (irModule: Module) =

    let simplifyBlock (index, blockValue) =
        match !blockValue with
        | { Content = BasicBlockValue block } ->
            // this optimization may be invalid if the basic block is used in a Phi. For now I'll be conservative and
            // not remove such basic blocks. As a future TODO I could check for semantic changes
            match block |> BasicBlock.instructions with
            | [ Ref { Content = GotoInstruction { BasicBlock = destination } } ] when not
                                                                                          (List.exists (fun x ->
                                                                                              match !x with
                                                                                              | { Content = PhiInstruction _ } ->
                                                                                                  true
                                                                                              | _ -> false)
                                                                                               (!blockValue).Users) ->
                blockValue |> Value.replaceWith destination
            | _ -> ()

            if index <> 0
               && !blockValue
                  |> BasicBlock.predecessors
                  |> List.isEmpty then
                blockValue |> Value.eraseFromParent
                false
            else
                true
        | _ -> failwith "Internal Compiler Error"


    irModule
    |> Module.basicBlocks
    |> List.indexed
    |> List.filter simplifyBlock
    |> List.map snd
    |> Module.fromBasicBlocks

let instructionCombine (irModule: Module) =
    let combine instruction =

        match instruction with
        | Ref { Content = UnaryInstruction { Kind = Shr
                                             Value = Ref { Content = UnaryInstruction { Kind = Shl
                                                                                        Value = passThrough }
                                                           Users = [ _ ] } as first } }
        | Ref { Content = UnaryInstruction { Kind = Shl
                                             Value = Ref { Content = UnaryInstruction { Kind = Shr
                                                                                        Value = passThrough }
                                                           Users = [ _ ] } as first } }
        | Ref { Content = UnaryInstruction { Kind = Not
                                             Value = Ref { Content = UnaryInstruction { Kind = Not
                                                                                        Value = passThrough }
                                                           Users = [ _ ] } as first } } ->
            instruction |> Value.replaceWith passThrough
            first |> Value.eraseFromParent
        | Ref { Content = BinaryInstruction { Kind = Add
                                              Left = Ref { Content = Constant { Value = value1 } }
                                              Right = Ref { Content = BinaryInstruction { Kind = Add
                                                                                          Left = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                            Users = [ _ ] } as first } }
        | Ref { Content = BinaryInstruction { Kind = Add
                                              Left = Ref { Content = Constant { Value = value1 } }
                                              Right = Ref { Content = BinaryInstruction { Kind = Add
                                                                                          Right = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                            Users = [ _ ] } as first } }
        | Ref { Content = BinaryInstruction { Kind = Add
                                              Right = Ref { Content = Constant { Value = value1 } }
                                              Left = Ref { Content = BinaryInstruction { Kind = Add
                                                                                         Right = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                           Users = [ _ ] } as first } }
        | Ref { Content = BinaryInstruction { Kind = Add
                                              Right = Ref { Content = Constant { Value = value1 } }
                                              Left = Ref { Content = BinaryInstruction { Kind = Add
                                                                                         Left = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                           Users = [ _ ] } as first } } ->
            first
            |> Value.replaceOperand oldOp (Builder.createConstant (value1 + value2))

            instruction |> Value.replaceWith first
        | Ref { Content = BinaryInstruction { Kind = And
                                              Left = Ref { Content = Constant { Value = value1 } }
                                              Right = Ref { Content = BinaryInstruction { Kind = And
                                                                                          Left = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                            Users = [ _ ] } as first } }
        | Ref { Content = BinaryInstruction { Kind = And
                                              Left = Ref { Content = Constant { Value = value1 } }
                                              Right = Ref { Content = BinaryInstruction { Kind = And
                                                                                          Right = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                            Users = [ _ ] } as first } }
        | Ref { Content = BinaryInstruction { Kind = And
                                              Right = Ref { Content = Constant { Value = value1 } }
                                              Left = Ref { Content = BinaryInstruction { Kind = And
                                                                                         Right = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                           Users = [ _ ] } as first } }
        | Ref { Content = BinaryInstruction { Kind = And
                                              Right = Ref { Content = Constant { Value = value1 } }
                                              Left = Ref { Content = BinaryInstruction { Kind = And
                                                                                         Left = Ref { Content = Constant { Value = value2 } } as oldOp }
                                                           Users = [ _ ] } as first } } ->
            first
            |> Value.replaceOperand oldOp (Builder.createConstant (value1 ||| value2))

            instruction |> Value.replaceWith first
        | Ref { Content = CondBrInstruction { Kind = NotZero
                                              Value = Ref { Users = [ _ ]
                                                            Content = BinaryInstruction { Right = Ref { Content = Constant { Value = 0x8000s } }
                                                                                          Left = passThrough } } as neg
                                              TrueBranch = trueBranch
                                              FalseBranch = falseBranch } }
        | Ref { Content = CondBrInstruction { Kind = NotZero
                                              Value = Ref { Users = [ _ ]
                                                            Content = BinaryInstruction { Left = Ref { Content = Constant { Value = 0x8000s } }
                                                                                          Right = passThrough } } as neg
                                              TrueBranch = trueBranch
                                              FalseBranch = falseBranch } } ->
            let newCond, _ =
                Builder.Default
                |> Builder.createCondBr Negative passThrough trueBranch falseBranch

            instruction |> Value.replaceWith newCond
            neg |> Value.eraseFromParent
        | _ -> ()

    irModule
    |> Module.instructions
    |> List.iter combine

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
    |> List.iter analyzeAlloc

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

        block
        |> BasicBlock.instructions
        |> List.filter (fun x ->
            match !x with
            | { Content = LoadInstruction { Source = Ref { Content = AllocationInstruction { Aliased = Some false } } } }
            | { Content = StoreInstruction { Destination = Ref { Content = AllocationInstruction { Aliased = Some false } } } } ->
                true
            | _ -> false)
        |> List.groupBy (fun x ->
            match !x with
            | { Content = LoadInstruction { Source = alloca } }
            | { Content = StoreInstruction { Destination = alloca } } -> alloca
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

let analyzeDominance (irModule: Module) =
    let map = Dictionary()

    irModule
    |> Module.basicBlocks
    |> List.map (fun x -> (x, None))
    |> List.iter map.Add

    let order = Dictionary()

    irModule
    |> Module.basicBlocks
    |> List.indexed
    |> List.map (fun (y, x) -> (x, y))
    |> List.iter order.Add

    let processSeq seq =
        map.[List.head seq] <- Some(List.head seq)

        let seq = List.skip 1 seq

        let intersect b1 b2 =
            let mutable finger1 = b1
            let mutable finger2 = b2

            while finger1 <> finger2 do
                while order.[finger1] > order.[finger2] do
                    match map.[finger1] with
                    | Some s -> finger1 <- s
                    | None -> failwith "Shouldn't be possible"

                while order.[finger2] > order.[finger1] do
                    match map.[finger2] with
                    | Some s -> finger2 <- s
                    | None -> failwith "Shouldn't be possible"

            finger1

        while seq
              |> List.fold (fun changed node ->
                  let processedPredecessors =
                      !node
                      |> BasicBlock.predecessors
                      |> List.choose (fun x -> map.[x] |> Option.map (fun value -> (x, value)))

                  match processedPredecessors with
                  | (newIDom, _) :: rest ->
                      let newIDom =
                          rest
                          |> List.map fst
                          |> List.fold intersect newIDom

                      if map.[node] <> Some newIDom then
                          map.[node] <- Some newIDom
                          true
                      else
                          changed
                  | [] ->
                      failwith
                          "Internal Compiler Error: Block with no predecessors found. Run simplifyCFG pass to eliminate")
                     false do
            ()

    irModule |> Module.basicBlocks |> processSeq

    irModule
    |> Module.basicBlocks
    |> List.iter (fun x ->
        let block = Value.asBasicBlock !x

        x
        := { !x with
                 Content =
                     BasicBlockValue
                         { block with
                               ImmediateDominator = map.[x] } })

    irModule

let analyzeDominanceFrontiers (irModule: Module) =

    let map = Dictionary()

    irModule
    |> Module.basicBlocks
    |> List.map (fun x -> (x, ImmutableHashSet.Create()))
    |> List.iter map.Add

    irModule
    |> Module.basicBlocks
    |> List.choose (fun x ->
        match !x |> BasicBlock.predecessors with
        | []
        | [ _ ] -> None
        | preds -> Some(x, preds))
    |> List.iter (fun (b, preds) ->
        let iDom =
            (!b
             |> Value.asBasicBlock
             |> BasicBlock.immediateDominator)

        preds
        |> List.iter (fun p ->

            Seq.unfold (fun runner ->
                if Some runner = iDom then
                    None
                else
                    map.[runner] <- map.[runner].Add(b)

                    match !runner
                          |> Value.asBasicBlock
                          |> BasicBlock.immediateDominator with
                    | None ->
                        failwith
                            "Internal Compiler Error: No immediate dominator calculated before analysing dominance frontiers"
                    | Some s -> Some((), s)) p
            |> Seq.tryLast
            |> ignore))

    irModule
    |> Module.basicBlocks
    |> List.iter (fun x ->
        let block = Value.asBasicBlock !x

        x
        := { !x with
                 Content =
                     BasicBlockValue
                         { block with
                               DominanceFrontier = Some(map.[x] |> List.ofSeq) } })

    irModule

let mem2reg (irModule: Module) =

    irModule
    |> Module.instructions
    |> List.choose (fun x ->
        match !x with
        | { Content = AllocationInstruction { Aliased = Some false } } ->
            Some(x, ImmutableHashSet.CreateRange(HashIdentity.Reference, (!x).Users))
        | _ -> None)
    |> List.iter (fun (alloca, loadStores) ->
        let s =
            loadStores
            |> Seq.choose (fun x ->
                match !x with
                | { Content = StoreInstruction _ } -> (!x).ParentBlock
                | _ -> None)
            |> Seq.distinct
            |> ImmutableHashSet.CreateRange

        let dominanceFrontiers (nodes: ImmutableHashSet<Value ref>): ImmutableHashSet<Value ref> =
            nodes
            |> Seq.map
                ((!)
                 >> Value.asBasicBlock
                 >> BasicBlock.dominanceFrontier)
            |> Seq.choose id
            |> Seq.map ImmutableHashSet.CreateRange
            |> Seq.reduce (fun x -> x.Union)

        let phis =
            Seq.unfold (fun (x: ImmutableHashSet<Value ref>) ->
                let next = (s.Union x) |> dominanceFrontiers
                if next.SetEquals(x) then None else Some(next, next)) s
            |> Seq.tryLast
            |> Option.map
                (Seq.map (fun block ->
                    let builder =
                        Builder.Default
                        |> Builder.setInsertBlock (Some block)
                        |> Builder.setInsertPoint Start

                    let phi =
                        builder
                        |> Builder.createPhi
                            (!block
                             |> BasicBlock.predecessors
                             |> List.map (fun x -> (Value.UndefValue, x)))
                        |> fst

                    match !alloca with
                    | { Content = AllocationInstruction { Register = Some r } } ->
                        match !phi with
                        | { Content = PhiInstruction phiInstruction } ->
                            phi
                            := { !phi with
                                     Content =
                                         PhiInstruction
                                             { phiInstruction with
                                                   Register = Some r } }
                        | _ -> ()
                    | _ -> ()

                    phi))

        let phis =
            match phis with
            | None -> ImmutableHashSet.Create()
            | Some s -> ImmutableHashSet.CreateRange(HashIdentity.Reference, s)

        let phiPredBlocks =
            phis
            |> Seq.choose (fun x ->
                (!x).ParentBlock
                |> Option.map ((!) >> BasicBlock.predecessors)
                |> Option.map (List.map (fun p -> (p, x))))
            |> Seq.concat
            |> Seq.groupBy fst
            |> Seq.map (fun (x, y) -> KeyValuePair(x, y |> List.ofSeq |> List.map snd))
            |> (fun x -> ImmutableDictionary.CreateRange(HashIdentity.Reference, x))

        irModule
        |> Module.instructions
        |> List.fold (fun replacement x ->
            match !x with
            | { Content = StoreInstruction { Value = passThrough } } when loadStores.Contains x ->
                x |> Value.eraseFromParent
                passThrough
            | { Content = LoadInstruction _ } when loadStores.Contains x ->
                x |> Value.replaceWith replacement
                replacement
            | { Content = PhiInstruction _ } when phis.Contains x -> x
            | _ when Value.isTerminating !x ->
                match (!x).ParentBlock with
                | None -> replacement
                | Some parentBlock ->
                    match phiPredBlocks.TryGetValue(parentBlock) with
                    | (false, _) -> replacement
                    | (true, phis) ->
                        phis
                        |> List.iter (fun phiValue ->
                            !phiValue
                            |> Value.operands
                            |> List.indexed
                            |> List.pairwise
                            |> List.filter (fun (_, (_, bb)) -> bb = parentBlock)
                            |> List.iter (fun ((i, _), _) -> phiValue |> Value.setOperand i replacement))

                        replacement
            | _ -> replacement) Value.UndefValue
        |> ignore

        alloca |> Value.eraseFromParent)

    irModule
