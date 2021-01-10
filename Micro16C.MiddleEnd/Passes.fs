module Micro16C.MiddleEnd.Passes

open System.Collections.Generic
open System.Collections.Immutable
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util


let instructionSimplify (irModule: Module ref) =

    let builder = Builder.fromModule irModule

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
        | { Content = CondBrInstruction { Kind = Zero
                                          Value = Ref { Content = Constant { Value = constant } }
                                          TrueBranch = trueBranch
                                          FalseBranch = falseBranch } } ->
            if constant = 0s then
                value
                |> Value.replaceWith (builder |> Builder.createGoto trueBranch |> fst)
            else
                value
                |> Value.replaceWith (builder |> Builder.createGoto falseBranch |> fst)
        | { Content = CondBrInstruction { Kind = Negative
                                          Value = Ref { Content = Constant { Value = constant } }
                                          TrueBranch = trueBranch
                                          FalseBranch = falseBranch } } ->
            if constant < 0s then
                value
                |> Value.replaceWith (builder |> Builder.createGoto trueBranch |> fst)
            else
                value
                |> Value.replaceWith (builder |> Builder.createGoto falseBranch |> fst)
        | { Content = PhiInstruction { Incoming = list } } when list
                                                                |> List.map fst
                                                                |> List.distinct
                                                                |> List.length = 1 ->
            value
            |> Value.replaceWith (list |> List.head |> fst)
        | _ -> ()

    !irModule
    |> Module.instructions
    |> List.iter simplify

    irModule

let deadCodeElimination (irModule: Module ref) =

    let eliminate value =
        if not (Value.hasSideEffects !value)
           && 0 = Value.useCount !value then
            value |> Value.eraseFromParent

    !irModule
    |> Module.instructions
    |> List.rev
    |> List.iter eliminate

    irModule

let simplifyCFG (irModule: Module ref) =

    let simplifyBlock (index, blockValue) =
        match !blockValue with
        | { Content = BasicBlockValue block } ->
            // this optimization may be invalid if the basic block is used in a Phi. For now I'll be conservative and
            // not remove such basic blocks. As a future TODO I could check for semantic changes
            match block |> BasicBlock.instructions with
            | [ Ref { Content = GotoInstruction { BasicBlock = destination } } ] when not
                                                                                          (List.exists (function
                                                                                              | Ref { Content = PhiInstruction _ } ->
                                                                                                  true
                                                                                              | _ -> false)
                                                                                               (!blockValue).Users) ->
                blockValue |> Value.replaceWith destination
            | _ ->
                if index <> 0
                   && !blockValue
                      |> BasicBlock.predecessors
                      |> List.isEmpty then
                    blockValue |> Value.eraseFromParent
        | _ -> failwith "Internal Compiler Error"

    !irModule
    |> Module.basicBlocks
    |> List.indexed
    |> List.iter simplifyBlock

    irModule

let instructionCombine (irModule: Module ref) =
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
        | Ref { Content = CondBrInstruction { Kind = Zero
                                              Value = Ref { Users = [ _ ]
                                                            Content = BinaryInstruction { Right = Ref { Content = Constant { Value = 0x8000s } }
                                                                                          Left = passThrough
                                                                                          Kind = And } } as neg
                                              TrueBranch = trueBranch
                                              FalseBranch = falseBranch } }
        | Ref { Content = CondBrInstruction { Kind = Zero
                                              Value = Ref { Users = [ _ ]
                                                            Content = BinaryInstruction { Left = Ref { Content = Constant { Value = 0x8000s } }
                                                                                          Right = passThrough
                                                                                          Kind = And } } as neg
                                              TrueBranch = trueBranch
                                              FalseBranch = falseBranch } } ->
            let newCond, _ =
                Builder.fromModule irModule
                |> Builder.createCondBr Negative passThrough falseBranch trueBranch

            instruction |> Value.replaceWith newCond
            neg |> Value.eraseFromParent
        | _ -> ()

    !irModule
    |> Module.instructions
    |> List.iter combine

    irModule

let analyzeAlloc (irModule: Module ref) =
    let analyzeAlloc instr =
        match !instr with
        | { Content = AllocationInstruction ({ Aliased = None } as alloca)
            Users = users } ->
            let addressTaken =
                users
                |> List.exists (function
                    | Ref { Content = LoadInstruction _ }
                    | Ref { Content = StoreInstruction _ } -> false
                    | _ -> true)

            instr
            := { !instr with
                     Content =
                         AllocationInstruction
                             { alloca with
                                   Aliased = Some addressTaken } }
        | _ -> ()

    !irModule
    |> Module.instructions
    |> List.iter analyzeAlloc

    irModule

let removeRedundantLoadStores (irModule: Module ref) =
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
            |> List.filter (function
                | Ref { Content = StoreInstruction _ } -> true
                | _ -> false)
            |> List.rev
            |> safeTail
            |> List.iter Value.eraseFromParent

        block
        |> BasicBlock.instructions
        |> List.filter (function
            | Ref { Content = LoadInstruction { Source = Ref { Content = AllocationInstruction { Aliased = Some false } } } }
            | Ref { Content = StoreInstruction { Destination = Ref { Content = AllocationInstruction { Aliased = Some false } } } } ->
                true
            | _ -> false)
        |> List.groupBy (function
            | Ref { Content = LoadInstruction { Source = alloca } }
            | Ref { Content = StoreInstruction { Destination = alloca } } -> alloca
            | _ -> failwith "Internal Compiler error")
        |> List.map (fun (_, x) ->
            match !(List.head x) with
            | { Content = LoadInstruction { Source = alloca } }
            | { Content = StoreInstruction { Destination = alloca } } -> (alloca, x)
            | _ -> failwith "Internal Compiler error")
        |> List.iter (snd >> simplifyLoadStoreSeries)

    !irModule
    |> Module.basicBlocks
    |> List.iter removeRedundantLoadStoresInBlock

    irModule

let analyzeDominance (irModule: Module ref) =
    let map = Dictionary(HashIdentity.Reference)

    !irModule
    |> Module.basicBlocks
    |> List.map (associateValue None)
    |> List.iter map.Add

    let order = Dictionary(HashIdentity.Reference)

    !irModule
    |> Module.basicBlocks
    |> List.indexed
    |> List.map (fun (y, x) -> (x, y))
    |> List.iter order.Add

    let processBlocks blocks =
        map.[List.head blocks] <- Some(List.head blocks)

        let blocks = List.skip 1 blocks

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

        while blocks
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

    !irModule |> Module.basicBlocks |> processBlocks

    !irModule
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

let analyzeDominanceFrontiers (irModule: Module ref) =

    let map = Dictionary(HashIdentity.Reference)

    !irModule
    |> Module.basicBlocks
    |> List.map (associateValue (ImmutableHashSet.Create<Value ref>(HashIdentity.Reference)))
    |> List.iter map.Add

    !irModule
    |> Module.basicBlocks
    |> List.choose (fun x ->
        match !x |> BasicBlock.predecessors with
        | []
        | [ _ ] -> None
        | preds -> Some(x, preds))
    |> List.iter (fun (b, preds) ->
        let iDom =
            !b
            |> Value.asBasicBlock
            |> BasicBlock.immediateDominator

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

    !irModule
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

let mem2reg (irModule: Module ref) =

    !irModule
    |> Module.instructions
    |> List.choose (fun x ->
        match !x with
        | { Content = AllocationInstruction { Aliased = Some false } } -> Some(x, (!x).Users |> ImmutableSet.ofList)
        | _ -> None)
    |> List.iter (fun (alloca, loadStores) ->
        let s =
            loadStores
            |> Seq.choose (function
                | Ref { Content = StoreInstruction _
                        ParentBlock = parentBlock } -> parentBlock
                | _ -> None)
            |> ImmutableSet.ofSeq

        let dominanceFrontiers =
            Seq.map
                ((!)
                 >> Value.asBasicBlock
                 >> BasicBlock.dominanceFrontier)
            >> Seq.choose id
            >> Seq.map ImmutableSet.ofList
            >> ImmutableSet.unionMany

        let builder = Builder.fromModule irModule

        let phis =
            Seq.unfold (fun (x: ImmutableHashSet<Value ref>) ->
                let next =
                    x |> ImmutableSet.union x |> dominanceFrontiers

                if next |> ImmutableSet.equal x then None else Some(next, next)) s
            |> Seq.tryLast
            |> Option.map
                (Seq.map (fun block ->
                    builder
                    |> Builder.setInsertBlock (Some block)
                    |> Builder.setInsertPoint Start
                    |> Builder.createPhi
                        (!block
                         |> BasicBlock.predecessors
                         |> List.map (fun x -> (Value.UndefValue, x)))
                    |> fst))
            |> Option.map ImmutableSet.ofSeq
            |> Option.defaultValue ImmutableSet.empty

        let phiPredBlocks =
            phis
            |> Seq.choose (fun x ->
                !x
                |> Value.parentBlock
                |> Option.map ((!) >> BasicBlock.predecessors)
                |> Option.map (List.map (associateValue x)))
            |> Seq.concat
            |> Seq.groupBy fst
            |> Seq.map (fun (x, y) -> (x, y |> List.ofSeq |> List.map snd))
            |> ImmutableMap.ofSeq

        let mutable alreadyVisited = ImmutableSet.empty

        let rec rename replacement blockValue =
            if ImmutableSet.contains blockValue alreadyVisited then
                ()
            else
                alreadyVisited <- ImmutableSet.add blockValue alreadyVisited

                let replacement =
                    !blockValue
                    |> Value.asBasicBlock
                    |> BasicBlock.instructions
                    |> List.fold (fun replacement x ->
                        match !x with
                        | { Content = StoreInstruction { Value = passThrough } } when loadStores
                                                                                      |> ImmutableSet.contains x ->
                            x |> Value.eraseFromParent
                            passThrough
                        | { Content = LoadInstruction _ } when loadStores |> ImmutableSet.contains x ->
                            x |> Value.replaceWith replacement
                            replacement
                        | { Content = PhiInstruction _ } when phis |> ImmutableSet.contains x -> x
                        | _ -> replacement) replacement

                phiPredBlocks
                |> ImmutableMap.tryFind blockValue
                |> Option.iter
                    (List.iter (fun phiValue ->
                        !phiValue
                        |> Value.operands
                        |> List.indexed
                        |> List.pairwise
                        |> List.filter (fun (_, (_, bb)) -> bb = blockValue)
                        |> List.iter (fun ((i, _), _) -> phiValue |> Value.setOperand i replacement)))

                !blockValue
                |> BasicBlock.successors
                |> List.iter (rename replacement)

        !irModule
        |> Module.basicBlocks
        |> List.tryHead
        |> Option.map (rename Value.UndefValue)
        |> ignore

        alloca |> Value.eraseFromParent)

    irModule

let numberAll (irModule: Module ref) =

    !irModule
    |> Module.instructions
    |> List.indexed
    |> List.iter (fun (index, value) -> value := { !value with Index = Some index })

    !irModule
    |> Module.basicBlocks
    |> List.indexed
    |> List.iter (fun (index, value) -> value := { !value with Index = Some index })

    irModule

type private LifeRange =
    | Done of int * int
    | NotDone of int * int

type private LifetimesDictionary = ImmutableDictionary<Value ref, LifeRange list>

let private distinctIntervals =
    List.sort
    >> List.fold (fun result (start, endV) ->
        match result with
        | [] -> [ (start, endV) ]
        | (prevStart, prevEnd) :: _ when start <= prevEnd -> (prevStart, max endV prevEnd) :: result
        | _ -> (start, endV) :: result) []
    >> List.rev

let private tryMaxBy p list =
    match list with
    | [] -> None
    | list -> list |> List.maxBy p |> Some

let analyzeLifetimes irModule =

    let map =
        ref (LifetimesDictionary.Empty.WithComparers HashIdentity.Reference)

    let analyzeLifetimesInBlock (block, prev) =

        match !block
              |> Value.asBasicBlock
              |> BasicBlock.tryTerminator
              |> Option.map ((!) >> Value.index) with
        | None -> ()
        | Some terminatorIndex ->
            // Adjust copy instructions that were generated by genPhiMoves so their lifetime ends at the terminator
            // Their lifetimes all need to overlap to correctly model the "parallel" moves that phis model
            let copiesAtEnd =
                !block
                |> Value.asBasicBlock
                |> BasicBlock.revInstructions
                |> List.skip 1
                |> List.takeWhile (function
                    | Ref { Content = CopyInstruction _
                            Users = [ Ref { Content = PhiInstruction _ } ] } -> true
                    | _ -> false)

            let copyEndIndex =
                copiesAtEnd
                |> List.tryHead
                |> Option.map ((!) >> Value.index)

            let copyStartIndex =
                copiesAtEnd
                |> List.tryLast
                |> Option.map ((!) >> Value.index)

            // Additionally add lifetime ranges for phi operations that are the targets of the copy operations.
            // These end at the last copy instruction, not at the terminator of the block like the copy operations.
            // this allows the register allocator to potentially reuse the same register for a phi and it's copy
            // operations in the case of a single phi in the successor
            (copyEndIndex, copyStartIndex)
            ||> Option.map2 (fun copyEndIndex copyStartIndex ->
                    !block
                    |> BasicBlock.successors
                    |> List.map ((!) >> Value.asBasicBlock >> BasicBlock.phis)
                    |> List.iter
                        (List.iter (fun phi ->
                            match !map |> ImmutableMap.tryFind phi with
                            | None ->
                                map
                                := !map
                                   |> ImmutableMap.add phi [ NotDone(copyStartIndex, terminatorIndex) ]
                            | Some ([] as rest)
                            | Some (Done _ :: _ as rest) ->

                                map
                                := !map
                                   |> ImmutableMap.add phi (NotDone(copyStartIndex, terminatorIndex) :: rest)
                            | Some (NotDone (start, endV) :: rest) ->
                                map
                                := !map
                                   |> ImmutableMap.add
                                       phi
                                          (NotDone(min start copyStartIndex, max endV terminatorIndex)
                                           :: rest))))
            |> ignore

        let revInstructions =
            !block
            |> Value.asBasicBlock
            |> BasicBlock.revInstructions

        revInstructions
        |> List.iter (fun instr ->
            let index, used =
                match !instr with
                | { Content = PhiInstruction _
                    ParentBlock = Some parentBlock } ->
                    (!parentBlock
                     |> Value.asBasicBlock
                     |> BasicBlock.nonPhiInstructions
                     |> List.head
                     |> (!)
                     |> Value.index,
                     [ instr ])
                | { Content = CopyInstruction { Source = value }
                    Users = [ Ref { Content = PhiInstruction _ } ] } -> (!instr |> Value.index, [ value ])
                | _ ->
                    (!instr |> Value.index,
                     (if Value.producesValue !instr then [ instr ] else [])
                     @ (!instr
                        |> Value.operands
                        |> List.filter ((!) >> Value.producesValue)))

            used
            |> List.iter (fun instr ->
                match !map |> ImmutableMap.tryFind instr with
                | Some [] -> failwith "Ought to not be possible"
                | Some (NotDone (_, endV) :: rest) ->
                    map
                    := !map
                       |> ImmutableMap.add instr (NotDone(index, endV) :: rest)
                | Some rest ->
                    map
                    := !map
                       |> ImmutableMap.add instr (NotDone(index, index) :: rest)
                | None ->
                    map
                    := !map
                       |> ImmutableMap.add instr [ NotDone(index, index) ])

            // turn the instr current range from NotDone into done
            match !instr with
            | { Content = PhiInstruction _ } -> ()
            | _ ->
                match (!map) |> ImmutableMap.tryFind instr with
                | None
                | Some []
                | Some (Done _ :: _) -> ()
                | Some (NotDone (start, endV) :: rest) ->
                    map
                    := !map
                       |> ImmutableMap.add instr (Done(start, endV) :: rest))

        // Handles loops. Iterate over all predecessors and check if any of them have a higher index, aka appear later
        // and are therefore connected over a back edge. We take the outer most loop of these and extend the lifetimes of
        // any values that are NotDone, meaning they have been used inside the loop, but are alive at the start of the loop,
        // to the end of the loop to make them available in every iteration.
        !block
        |> BasicBlock.predecessors
        |> List.filter
            ((!)
             >> Value.index
             >> ((<) (!block |> Value.index)))
        |> tryMaxBy ((!) >> Value.index)
        |> Option.iter (fun backEdgeBlock ->
            let rangeEnd =
                !backEdgeBlock
                |> Value.asBasicBlock
                |> BasicBlock.revInstructions
                |> List.head
                |> (!)
                |> Value.index

            map
            := !map
               |> ImmutableMap.map (function
                   | value, ([] as lifetimes)
                   | value, (Done _ :: _ as lifetimes) -> (value, lifetimes)
                   | value, NotDone (start, _) :: rest -> (value, NotDone(start, rangeEnd) :: rest)))

    //        prev
//        |> Option.filter (fun prev ->
//            !block
//            |> BasicBlock.successors
//            |> List.contains prev
//            |> not)
//        |> Option.iter (fun prev ->
//            // the previous block was not a successor of this block. If any uses appeared in the previous block
//            // that did not appear in this block then we have a lifetime hole. That means we need to set the start
//            // of the interval to the beginning of the previous block and make it Done
//            let prevStart =
//                !prev
//                |> Value.asBasicBlock
//                |> BasicBlock.instructions
//                |> List.tryHead
//                |> Option.map ((!) >> Value.index)
//
//            (revInstructions
//             |> List.tryHead
//             |> Option.map ((!) >> Value.index),
//             prevStart)
//            ||> Option.map2 (fun blockEnd prevStart ->
//                    map
//                    := !map
//                       |> ImmutableMap.map (function
//                           | value, ([] as lifeRange)
//                           | value, (Done _ :: _ as lifeRange) -> (value, lifeRange)
//                           | value, (NotDone (lastUse, _) :: _ as lifeRange) when lastUse <= blockEnd ->
//                               (value, lifeRange)
//                           | value, NotDone (_, endV) :: rest -> (value, Done(prevStart, endV) :: rest)))
//            |> ignore)

    !irModule
    |> Module.revBasicBlocks
    |> List.fold (fun x y ->
        match x with
        | [] -> [ (y, None) ]
        | ((z, _) :: _) as list -> (y, Some z) :: list) []
    |> List.rev
    |> List.iter analyzeLifetimesInBlock

    !map
    |> ImmutableMap.iter (fun (value, lifetimes) ->
        value
        := { !value with
                 LifeIntervals =
                     lifetimes
                     |> List.map (function
                         | Done (x, y) -> (x, y)
                         | NotDone (x, y) -> (x, y))
                     |> distinctIntervals })

    irModule
