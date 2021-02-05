module Micro16C.MiddleEnd.Passes

open System.Collections.Generic
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util
open Micro16C.MiddleEnd.PassManager

let private singleInstructionSimplify builder value =
    match value with
    // And patterns
    | BinOp And
            ((ConstOp 0s
             | UndefOp),
             _
            | _,
              (ConstOp 0s
              | UndefOp)) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    | BinOp And
            (ConstOp -1s, passThrough
            | passThrough, ConstOp -1s) ->
        value |> Value.replaceWith passThrough
        true
    | BinOp And (lhs, rhs) when lhs = rhs ->
        value |> Value.replaceWith lhs
        true
    | BinOp And
            (other1, UnaryOp Not other2
            | UnaryOp Not other2, other1) when other1 = other2 ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    | BinOp And
            (other1,
             BinOp Or
                   (other2, _
                   | _, other2)
            | BinOp Or
                    (other2, _
                    | _, other2),
              other1) when other1 = other2 ->
        value |> Value.replaceWith other1

        true
    // Or patterns
    | BinOp Or
            ((ConstOp -1s
             | UndefOp),
             _
            | _,
              (ConstOp -1s
              | UndefOp)) ->
        value
        |> Value.replaceWith (Builder.createConstant -1s)

        true
    // X | X = X
    | BinOp Or (lhs, rhs) when rhs = lhs ->
        value |> Value.replaceWith rhs

        true
    // X | 0 = X
    | BinOp Or
            (x, ConstOp 0s
            | ConstOp 0s, x) ->
        value |> Value.replaceWith x

        true
    // A | ~A  =  ~A | A  =  -1
    | BinOp Or
            (a1, UnaryOp Not a2
            | UnaryOp Not a2, a1) when a1 = a2 ->
        value
        |> Value.replaceWith (Builder.createConstant -1s)

        true
    // A | (A & ?) = A
    // (A & ?) | A = A
    | BinOp Or
            (a1, BinOp And (a2, a3)
            | BinOp And (a2, a3), a1) when a1 = a2 || a1 = a3 ->
        value |> Value.replaceWith a1

        true
    // A | ~(A & ?) = -1
    // ~(A & ?) | A = -1
    | BinOp Or
            (a1, UnaryOp Not (BinOp And (a2, a3))
            | UnaryOp Not (BinOp And (a2, a3)), a1) when a1 = a2 || a1 = a3 ->
        value
        |> Value.replaceWith (Builder.createConstant -1s)

        true
    // Xor patterns
    // X ^ undef -> undef
    | BinOp Xor (_, UndefOp) ->
        value |> Value.replaceWith Value.UndefValue

        true
    // X ^ 0 -> X
    | BinOp Xor (x, ConstOp 0s) ->
        value |> Value.replaceWith x

        true
    // X ^ X -> 0
    | BinOp Xor (x1, x2) when x1 = x2 ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X ^ ~X = ~X ^ X = -1
    | BinOp Xor
            (x1, UnaryOp Not x2
            | UnaryOp Not x1, x2) when x1 = x2 ->
        value
        |> Value.replaceWith (Builder.createConstant -1s)

        true
    // Add patterns
    // X + undef -> undef
    | BinOp Add
            (UndefOp, _
            | UndefOp, _) ->
        value |> Value.replaceWith Value.UndefValue
        true
    // X + 0 -> X
    | BinOp Add
            (passThrough, ConstOp 0s
            | ConstOp 0s, passThrough) ->
        value |> Value.replaceWith passThrough
        true
    // X + X -> lsh(X)
    | BinOp Add (lhs, rhs) when lhs = rhs ->
        value
        |> Value.replaceWith
            (builder
             |> Builder.createBinary lhs Shl (Builder.createConstant 1s)
             |> fst)

        true
    // X + (Y - X) -> Y
    // (Y - X) + X -> Y
    | BinOp Add
            (x1, BinOp Sub (y, x2)
            | BinOp Sub (y, x2), x1) when x1 = x2 ->
        value |> Value.replaceWith y
        true
    | BinOp Add
            (x1, UnaryOp Negate x2
            | UnaryOp Negate x1, x2) when x1 = x2 ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // mul patterns
    // X * undef -> 0
    // X * 0 -> 0
    | BinOp Mul
            ((ConstOp 0s
             | UndefOp),
             _
            | (ConstOp 0s
              | UndefOp),
              _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X * 1 -> X
    | BinOp Mul
            (ConstOp 1s, x
            | x, ConstOp 1s) ->
        value |> Value.replaceWith x

        true
    // (X / Y) * Y -> X
    | BinOp Mul
            (y1, BinOp SDiv (x, y2)
            | BinOp SDiv (x, y2), y1)
    | BinOp Mul
            (y1, BinOp UDiv (x, y2)
            | BinOp UDiv (x, y2), y1) when y1 = y2 ->
        value |> Value.replaceWith x

        true
    // Div patterns
    // undef / X -> 0
    | BinOp UDiv (UndefOp, _)
    | BinOp SDiv (UndefOp, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // 0 / X -> 0
    | BinOp UDiv (ConstOp 0s, _)
    | BinOp SDiv (ConstOp 0s, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X / X -> 1
    | BinOp UDiv (x1, x2)
    | BinOp SDiv (x1, x2) when x1 = x2 ->
        value
        |> Value.replaceWith (Builder.createConstant 1s)

        true
    // X / 1 -> X
    | BinOp UDiv (x, ConstOp 1s)
    | BinOp SDiv (x, ConstOp 1s) ->
        value |> Value.replaceWith x

        true
    // (X rem Y) / Y -> 0
    | BinOp UDiv
            (BinOp SRem (_, y1), y2
            | BinOp URem (_, y1), y2)
    | BinOp SDiv
            (BinOp SRem (_, y1), y2
            | BinOp URem (_, y1), y2) when y1 = y2 ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // rem patterns
    // (X % Y) % Y -> X % Y
    | BinOp SRem (BinOp SRem (_, y1) as op, y2) when y1 = y2 ->
        value |> Value.replaceWith op

        true
    | BinOp URem (BinOp URem (_, y1) as op, y2) when y1 = y2 ->
        value |> Value.replaceWith op

        true
    // Not patterns
    // ~(~X) -> X
    | UnaryOp Not (UnaryOp Not x) ->
        value |> Value.replaceWith x
        true
    // Negate patterns
    // -(-X) -> X
    | UnaryOp Negate (UnaryOp Negate x) ->
        value |> Value.replaceWith x
        true
    // Shl patterns
    // 0 << X -> 0
    | BinOp Shl (ConstOp 0s, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X << 0 -> X
    | BinOp Shl (x, ConstOp 0s) ->
        value |> Value.replaceWith x

        true
    // undef << X -> 0
    | BinOp Shl (UndefOp, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X << c -> 0 when c >= 16
    | BinOp Shl (_, ConstOp c) when c >= 16s ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // lshr patterns
    // 0 >> X -> 0
    | BinOp LShr (ConstOp 0s, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X >> 0 -> X
    | BinOp LShr (x, ConstOp 0s) ->
        value |> Value.replaceWith x

        true
    // X >> c -> 0 when c >= 16
    | BinOp LShr (_, ConstOp c) when c >= 16s ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // ashr patterns
    // 0 >> X -> 0
    | BinOp AShr (ConstOp 0s, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X >> 0 -> X
    | BinOp AShr (x, ConstOp 0s) ->
        value |> Value.replaceWith x

        true
    // -1 >> X -> -1
    | BinOp AShr (ConstOp -1s, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    | CondBrOp (_, _, trueBranch, falseBranch) when trueBranch = falseBranch ->
        value
        |> Value.replaceWith (builder |> Builder.createGoto trueBranch |> fst)

        true
    | PhiOp list when list
                      |> List.map fst
                      |> List.distinct
                      |> List.length = 1 ->
        value
        |> Value.replaceWith (list |> List.head |> fst)

        true
    | _ -> false

let private instructionSimplify _ (irModule: Module ref) =

    let builder = Builder.fromModule irModule

    while !irModule
          |> Module.instructions
          |> List.map (singleInstructionSimplify builder)
          |> List.exists id do
        ()

    irModule

let private deadCodeElimination _ (irModule: Module ref) =

    let eliminate value =
        if not (Value.hasSideEffects !value)
           && 0 = Value.useCount !value then
            value |> Value.destroy
            true
        else
            false

    while !irModule
          |> Module.revInstructions
          |> List.map eliminate
          |> List.exists id do
        ()

    irModule

let private simplifyCFG _ (irModule: Module ref) =

    let simplifyBlock blockValue =
        if !blockValue |> Value.isBasicBlock |> not then
            // As we may delete a successor this case could occur
            false
        else
            let block = !blockValue |> Value.asBasicBlock
            // this optimization may be invalid if the basic block is used in a Phi. For now I'll be conservative and
            // not remove such basic blocks. As a future TODO I could check for semantic changes
            match block |> BasicBlock.revInstructions with
            | [ GotoOp destination ] when !blockValue
                                          |> Value.users
                                          |> List.exists (function
                                              | PhiOp _ -> true
                                              | _ -> false)
                                          |> not ->
                blockValue |> Value.replaceWith destination
                true
            | GotoOp destination as terminator :: _ when (!destination |> BasicBlock.hasSinglePredecessor)
                                                         && (!destination
                                                             |> Value.asBasicBlock
                                                             |> BasicBlock.phis
                                                             |> List.isEmpty) ->
                terminator |> Value.destroy

                let builder =
                    Builder.fromModule irModule
                    |> Builder.setInsertBlock (Some blockValue)
                    |> Builder.setInsertPoint End

                !destination
                |> Value.asBasicBlock
                |> BasicBlock.instructions
                |> List.iter (fun x -> builder |> Builder.insertValue x |> ignore)

                destination |> Value.replaceWith blockValue
                true
            | _ -> false

    while !irModule
          |> Module.basicBlocks
          |> List.map simplifyBlock
          |> List.exists id do
        ()

    irModule

let private removeUnreachableBlocks _ (irModule: Module ref) =

    let set =
        !irModule
        |> Module.preOrder
        |> Seq.fold (fun set bb -> set |> ImmutableSet.add bb) ImmutableSet.empty

    !irModule
    |> Module.revBasicBlocks
    |> Seq.filter (fun v -> ImmutableSet.contains v set |> not)
    |> Seq.iter Value.destroy

    irModule

let private localConstantFolding builder instructions =
    instructions
    |> List.iter (fun value ->
        match value with
        | BinOp And (ConstOp lhs, ConstOp rhs) ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs &&& rhs))
        | BinOp Or (ConstOp lhs, ConstOp rhs) ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs ||| rhs))
        | BinOp Xor (ConstOp c1, ConstOp c2) ->
            value
            |> Value.replaceWith (Builder.createConstant (c1 ^^^ c2))
        | BinOp Add (ConstOp lhs, ConstOp rhs) ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs + rhs))
        | BinOp Mul (ConstOp c1, ConstOp c2) ->
            value
            |> Value.replaceWith (Builder.createConstant (c1 * c2))
        | BinOp SDiv (ConstOp c1, ConstOp c2) ->
            value
            |> Value.replaceWith (Builder.createConstant (c1 / c2))
        | BinOp UDiv (ConstOp c1, ConstOp c2) ->
            value
            |> Value.replaceWith (Builder.createConstant (int16 ((c1 |> uint16) / (c2 |> uint16))))
        | BinOp URem (ConstOp c1, ConstOp c2) ->
            value
            |> Value.replaceWith (Builder.createConstant (((c1 |> uint16) % (c2 |> uint16)) |> int16))

        | BinOp SRem (ConstOp c1, ConstOp c2) ->
            value
            |> Value.replaceWith (Builder.createConstant (c1 % c2))
        | UnaryOp Not (ConstOp c) ->
            value
            |> Value.replaceWith (Builder.createConstant (~~~c))
        | UnaryOp Negate (ConstOp c) ->
            value
            |> Value.replaceWith (Builder.createConstant -c)
        | BinOp Shl (ConstOp lhs, ConstOp rhs) ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs <<< (rhs |> int32)))
        | BinOp LShr (ConstOp lhs, ConstOp rhs) ->
            value
            |> Value.replaceWith (Builder.createConstant ((lhs |> uint16) >>> (rhs |> int32) |> int16))
        | BinOp AShr (ConstOp lhs, ConstOp rhs) ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs >>> (rhs |> int)))
        | CondBrOp (Zero, ConstOp c, trueBranch, falseBranch) ->
            if c = 0s then
                value
                |> Value.replaceWith (builder |> Builder.createGoto trueBranch |> fst)
            else
                value
                |> Value.replaceWith (builder |> Builder.createGoto falseBranch |> fst)
        | CondBrOp (Negative, ConstOp c, trueBranch, falseBranch) ->
            if c < 0s then
                value
                |> Value.replaceWith (builder |> Builder.createGoto trueBranch |> fst)
            else
                value
                |> Value.replaceWith (builder |> Builder.createGoto falseBranch |> fst)
        | _ -> ())

let private jumpThreading _ irModule =

    let builder = Builder.fromModule irModule

    let jumpThreadingBlock blockValue =
        let block = !blockValue |> Value.asBasicBlock

        let phis = block |> BasicBlock.phis

        let escapingValues =
            block
            |> BasicBlock.revInstructions
            |> List.filter ((!) >> Value.producesValue)
            |> List.filter
                ((!)
                 >> Value.users
                 >> List.exists ((!) >> Value.parentBlock >> (<>) (Some blockValue)))

        let createCopies pred =
            let newBlock, builder =
                builder
                |> Builder.createBasicBlockAt (After pred) ((!blockValue |> Value.name) + ".copy")

            phis
            |> List.fold (fun result phi ->
                Seq.unfold (function
                    | PhiOp _ as phi ->
                        let replacement =
                            !phi
                            |> Value.operands
                            |> List.pairwise
                            |> List.find (snd >> (=) pred)
                            |> fst

                        if replacement = phi then
                            Some(Value.UndefValue, Value.UndefValue)
                        else
                            Some(replacement, replacement)
                    | _ -> None) phi
                |> Seq.skipWhile ((!) >> Value.parentBlock >> (=) (Some blockValue))
                |> Seq.tryHead
                |> Option.map (fun x -> (phi, x))
                |> Option.map2 (fun x y -> y :: x) result) (Some [])
            |> Option.map ImmutableMap.ofList
            |> Option.map (fun replacements ->
                builder
                |> Builder.setInsertBlock (Some newBlock)
                |> Builder.copyInstructionsStructure replacements (block |> BasicBlock.instructions))
            |> Option.map (fun replacement -> (newBlock, replacement))

        let copies =
            !blockValue
            |> BasicBlock.predecessors
            |> List.choose (fun pred ->
                let newBlock = createCopies pred |> Option.map fst

                newBlock
                |> Option.iter
                    ((!)
                     >> Value.asBasicBlock
                     >> BasicBlock.instructions
                     >> localConstantFolding builder)

                newBlock |> Option.map (fun x -> (x, pred)))

        let instructionCount =
            lazy
                ((copies
                  |> List.sumBy
                      (fst
                       >> (!)
                       >> Value.asBasicBlock
                       >> BasicBlock.revInstructions
                       >> List.length))
                 - List.length copies
                 + ((1
                     + (!blockValue
                        |> BasicBlock.successors
                        |> List.length))
                    * List.length escapingValues))
        // doing -1 for each block as worst case scenario one terminator can always be removed
        // also + 1 * (amount of successors + 1) for every value that escapes due to having to create phis in every
        // successor and having to merge them as well. (worst case scenario really)

        let killedConditional =
            lazy
                (block
                 |> BasicBlock.tryTerminator
                 |> Option.map ((!) >> Value.isUnconditional >> not)
                 |> Option.defaultValue false
                 && copies
                    |> List.exists
                        (fst
                         >> (!)
                         >> Value.asBasicBlock
                         >> BasicBlock.tryTerminator
                         >> Option.filter (function
                             | GotoOp _ -> false
                             | _ -> true)
                         >> Option.isNone))

        if copies |> List.length = (!blockValue
                                    |> BasicBlock.predecessors
                                    |> List.length)
           && (killedConditional.Force()
               || instructionCount.Force()
                  <= (block |> BasicBlock.revInstructions |> List.length)) then

            let shareSingleSuccessor =
                lazy
                    (!blockValue
                     |> BasicBlock.successors
                     |> List.distinctBy ((!) >> BasicBlock.successors)
                     |> (fun x ->
                         x |> List.length = 1
                         && !x.[0] |> BasicBlock.hasSingleSuccessor))

            if escapingValues |> List.isEmpty then
                // If we have no escaping values then no phis need to be created in the successor and we can actually
                // use the already optimized blocks we previously calculated instruction count with
                copies
                |> List.iter (fun (newBlock, pred) ->
                    !pred
                    |> Value.asBasicBlock
                    |> BasicBlock.terminator
                    |> Value.replaceOperand blockValue newBlock)

                blockValue |> Value.destroy
                true
            else

            if !blockValue |> BasicBlock.hasSingleSuccessor then
                let mergeBlock =
                    !blockValue
                    |> BasicBlock.successors
                    |> List.exactlyOne

                copies |> List.iter (fst >> Value.destroy)

                let copies =
                    !blockValue
                    |> BasicBlock.predecessors
                    |> List.map (fun pred ->
                        let newBlock, replacements = createCopies pred |> Option.get

                        !pred
                        |> Value.asBasicBlock
                        |> BasicBlock.terminator
                        |> Value.replaceOperand blockValue newBlock

                        (newBlock, replacements))

                escapingValues
                |> List.iter (fun oldValue ->
                    let incomingList =
                        !mergeBlock
                        |> BasicBlock.predecessors
                        |> List.map (fun block ->
                            match copies |> List.tryFind (fst >> (=) block) with
                            | None -> (Value.UndefValue, block)
                            | Some (_, replacements) -> (replacements |> ImmutableMap.find oldValue, block))

                    let phi =
                        builder
                        |> Builder.setInsertBlock (Some mergeBlock)
                        |> Builder.setInsertPoint Start
                        |> Builder.createPhi incomingList
                        |> fst

                    oldValue |> Value.replaceWith phi)

                blockValue |> Value.destroy
                true
            else if shareSingleSuccessor.Force() then
                // Otherwise we need to create phi nodes that will have the replacements for the escaping values as
                // incoming values for the new blocks.
                // Since due to constant folding or whatever optimizations that we applied to estimate instruction count
                // after jump threading, the
                // replacement value may have been destroyed and it's too hard to track we are simply going to redo the
                // process, not optimize this time and link the replacement into the phi we'll be creating in the
                // successor of the new block which are distinct as assured in the filter below

                copies |> List.iter (fst >> Value.destroy)

                let copies =
                    !blockValue
                    |> BasicBlock.predecessors
                    |> List.map (fun pred ->
                        let newBlock, replacements = createCopies pred |> Option.get

                        !pred
                        |> Value.asBasicBlock
                        |> BasicBlock.terminator
                        |> Value.replaceOperand blockValue newBlock

                        (newBlock, replacements))

                let mergeBlock =
                    !blockValue
                    |> BasicBlock.successors
                    |> List.head
                    |> (!)
                    |> BasicBlock.successors
                    |> List.exactlyOne

                escapingValues
                |> List.iter (fun oldValue ->
                    let incomingList bb =
                        !bb
                        |> BasicBlock.predecessors
                        |> List.map (fun block ->
                            match copies |> List.tryFind (fst >> (=) block) with
                            | None -> (Value.UndefValue, block)
                            | Some (_, replacements) -> (replacements |> ImmutableMap.find oldValue, block))

                    let mergeIncoming =
                        !blockValue
                        |> BasicBlock.successors
                        |> List.map (fun bb ->
                            let phi =
                                builder
                                |> Builder.setInsertBlock (Some bb)
                                |> Builder.setInsertPoint Start
                                |> Builder.createPhi (incomingList bb)
                                |> fst

                            !oldValue
                            |> Value.users
                            |> List.filter ((!) >> Value.parentBlock >> (=) (Some bb))
                            |> List.iter (Value.replaceOperand oldValue phi)

                            (phi, bb))

                    let phi =
                        builder
                        |> Builder.setInsertBlock (Some mergeBlock)
                        |> Builder.setInsertPoint Start
                        |> Builder.createPhi mergeIncoming
                        |> fst

                    oldValue |> Value.replaceWith phi)

                blockValue |> Value.destroy
                true
            else
                copies |> List.iter (fst >> Value.destroy)
                false
        else
            copies |> List.iter (fst >> Value.destroy)
            false


    // Only run on basic blocks that have more than one predecessor and contain a phi. Without phis this could only
    // generate more code
    while !irModule
          |> Module.basicBlocks
          |> Seq.filter (fun blockValue ->
              !blockValue
              |> BasicBlock.predecessors
              |> List.length > 1
              && !blockValue
                 |> Value.asBasicBlock
                 |> BasicBlock.phis
                 |> List.isEmpty
                 |> not)
          |> Seq.map jumpThreadingBlock
          |> List.ofSeq
          |> List.exists id do
        ()

    irModule

let private singleInstructionCombine builder value =
    match value with
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
                                          Left = Ref { Content = Constant { Value = value1 } }
                                          Right = Ref { Content = BinaryInstruction { Kind = Add
                                                                                      Left = Ref { Content = Constant { Value = value2 } } as oldOp }
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

        value |> Value.replaceWith first
        true
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

        value |> Value.replaceWith first
        true
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
            builder
            |> Builder.createCondBr Negative passThrough falseBranch trueBranch

        value |> Value.replaceWith newCond
        neg |> Value.destroy
        true
    | Ref { Content = BinaryInstruction { Kind = Add
                                          Right = op1
                                          Left = Ref { Content = UnaryInstruction { Kind = Not; Value = op2 } } } }
    | Ref { Content = BinaryInstruction { Kind = Add
                                          Left = op1
                                          Right = Ref { Content = UnaryInstruction { Kind = Not; Value = op2 } } } } when op1 = op2 ->
        value
        |> Value.replaceWith (Builder.createConstant 0xFFFFs)

        true
    | Ref { Content = BinaryInstruction { Kind = And
                                          Right = op1
                                          Left = Ref { Content = UnaryInstruction { Kind = Not; Value = op2 } } } }
    | Ref { Content = BinaryInstruction { Kind = And
                                          Left = op1
                                          Right = Ref { Content = UnaryInstruction { Kind = Not; Value = op2 } } } } when op1 = op2 ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    | Ref { Content = BinaryInstruction { Kind = Add
                                          Right = op1
                                          Left = Ref { Content = BinaryInstruction { Kind = Add
                                                                                     Right = Ref { Content = UnaryInstruction { Kind = Not
                                                                                                                                Value = op2 } }
                                                                                     Left = Ref { Content = Constant { Value = 1s } } } } } }
    | Ref { Content = BinaryInstruction { Kind = Add
                                          Right = op1
                                          Left = Ref { Content = BinaryInstruction { Kind = Add
                                                                                     Left = Ref { Content = UnaryInstruction { Kind = Not
                                                                                                                               Value = op2 } }
                                                                                     Right = Ref { Content = Constant { Value = 1s } } } } } }
    | Ref { Content = BinaryInstruction { Kind = Add
                                          Left = op1
                                          Right = Ref { Content = BinaryInstruction { Kind = Add
                                                                                      Right = Ref { Content = UnaryInstruction { Kind = Not
                                                                                                                                 Value = op2 } }
                                                                                      Left = Ref { Content = Constant { Value = 1s } } } } } }
    | Ref { Content = BinaryInstruction { Kind = Add
                                          Left = op1
                                          Right = Ref { Content = BinaryInstruction { Kind = Add
                                                                                      Left = Ref { Content = UnaryInstruction { Kind = Not
                                                                                                                                Value = op2 } }
                                                                                      Right = Ref { Content = Constant { Value = 1s } } } } } } when op1 = op2 ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    | _ -> false

let private instructionCombine _ (irModule: Module ref) =

    let builder = Builder.fromModule irModule

    while !irModule
          |> Module.instructions
          |> List.map (singleInstructionCombine builder)
          |> List.exists id do
        ()

    irModule

let analyzeAlloc _ (irModule: Module ref) =

    !irModule
    |> Module.instructions
    |> List.fold (fun result instr ->
        match !instr with
        | { Content = AllocationInstruction
            Users = users } ->
            if users
               |> List.exists (function
                   | LoadOp _ -> false
                   | StoreOp (value, _) when value <> instr -> false
                   | _ -> true) then
                result |> ImmutableSet.add instr
            else
                result
        | _ -> result) ImmutableSet.empty

let analyzeAllocPass = { Pass = analyzeAlloc; DependsOn = [] }

type DominanceInfo =
    { ImmediateDominator: Value ref
      ImmediatelyDominates: Value ref list
      DominanceFrontier: Value ref list }

let analyzeDominance _ (irModule: Module ref) =
    let map = Dictionary(HashIdentity.Reference)

    // as seen in https://www.cs.rice.edu/~keith/Embed/dom.pdf

    !irModule
    |> Module.basicBlocks
    |> List.map (associateValue None)
    |> List.iter map.Add

    let processBlocks blocks =
        map.[Seq.head blocks] <- Some(Seq.head blocks)

        let order =
            blocks
            |> Seq.indexed
            |> Seq.fold (fun map (index, value) -> ImmutableMap.add value index map) ImmutableMap.empty

        let blocks = Seq.skip 1 blocks

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
              |> Seq.fold (fun changed node ->
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

    !irModule
    |> Module.reversePostOrder
    |> Seq.cache
    |> processBlocks

    let result =
        !irModule
        |> Module.revBasicBlocks
        |> List.fold (fun result x ->
            let block = Value.asBasicBlock !x

            let iDom = map.[x]

            match iDom with
            | None -> result
            | Some iDom ->
                result
                |> ImmutableMap.add
                    x
                       { ImmediateDominator = iDom
                         ImmediatelyDominates = []
                         DominanceFrontier = [] }) ImmutableMap.empty

    result
    |> Seq.fold (fun result kv ->
        let x, { ImmediateDominator = iDom } = kv.Deconstruct()
        let domInfo = result |> ImmutableMap.find iDom

        result
        |> ImmutableMap.add
            iDom
               { domInfo with
                     ImmediatelyDominates = x :: (domInfo.ImmediatelyDominates) }) result

let analyzeDominancePass =
    { Pass = analyzeDominance
      DependsOn = [] }

let private analyzeDominanceFrontiers passManager (irModule: Module ref) =

    let domInfos =
        passManager
        |> PassManager.analysisData analyzeDominancePass

    let map = Dictionary(HashIdentity.Reference)

    !irModule
    |> Module.revBasicBlocks
    |> List.map (associateValue (ImmutableSet.empty))
    |> List.iter map.Add

    !irModule
    |> Module.revBasicBlocks
    |> List.choose (fun x ->
        match !x |> BasicBlock.predecessors with
        | []
        | [ _ ] -> None
        | preds -> Some(x, preds))
    |> List.iter (fun (b, preds) ->
        let iDom = domInfos.[b].ImmediateDominator

        preds
        |> List.iter (fun p ->
            Seq.unfold (fun runner ->
                if runner = iDom then
                    None
                else
                    map.[runner] <- map.[runner] |> ImmutableSet.add b
                    Some((), domInfos.[runner].ImmediateDominator)) p
            |> Seq.tryLast
            |> ignore))

    domInfos
    |> Seq.map (fun kv ->
        let x, info = kv.Deconstruct()

        (x,
         { info with
               DominanceFrontier = map.[x] |> List.ofSeq }))
    |> ImmutableMap.ofSeq

let analyzeDominanceFrontiersPass =
    { Pass = analyzeDominanceFrontiers
      DependsOn = [ analyzeDominancePass ] }

let private mem2reg passManager (irModule: Module ref) =

    let aliased =
        passManager
        |> PassManager.analysisData analyzeAllocPass

    let domInfo =
        passManager
        |> PassManager.analysisData analyzeDominanceFrontiersPass

    !irModule
    |> Module.instructions
    |> List.choose (fun x ->
        match x with
        | AllocaOp when aliased |> ImmutableSet.contains x |> not -> Some(x, (!x).Users |> ImmutableSet.ofList)
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
            Seq.map (fun x -> domInfo.[x].DominanceFrontier)
            >> Seq.map ImmutableSet.ofList
            >> ImmutableSet.unionMany

        let builder = Builder.fromModule irModule

        let phis =
            Seq.unfold (fun (x: ImmutableSet<Value ref>) ->
                let next =
                    s |> ImmutableSet.union x |> dominanceFrontiers

                if next = x then None else Some(next, next)) s
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
                        match x with
                        | StoreOp (passThrough, _) when loadStores |> ImmutableSet.contains x ->
                            x |> Value.destroy
                            passThrough
                        | LoadOp _ when loadStores |> ImmutableSet.contains x ->
                            x |> Value.replaceWith replacement
                            replacement
                        | PhiOp _ when phis |> ImmutableSet.contains x -> x
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
        |> Module.entryBlock
        |> Option.map (rename Value.UndefValue)
        |> ignore

        alloca |> Value.destroy)

    irModule

type LivenessInfo =
    { LiveIn: ImmutableSet<Value ref>
      LiveOut: ImmutableSet<Value ref> }

[<RequireQualifiedAccess>]
module LivenessInfo =

    let liveOut info = info.LiveOut

    let liveIn info = info.LiveIn

let analyzeLiveness _ irModule =

    let mutable result = ImmutableMap.empty

    !irModule
    |> Module.backwardAnalysis
        (fun liveOut blockValue ->
            let block = !blockValue |> Value.asBasicBlock

            // As seen in https://hal.inria.fr/inria-00558509v2/document

            let liveIn =
                block
                |> BasicBlock.revInstructions
                |> List.fold (fun set instr ->

                    let set, upwardsExposed =
                        match instr with
                        | PhiOp _ -> (set, [ instr ])
                        | _ ->
                            let set =
                                if !instr |> Value.producesValue then set |> ImmutableSet.remove instr else set

                            (set,
                             !instr
                             |> Value.operands
                             |> List.filter ((!) >> Value.isInstruction))

                    set
                    |> ImmutableSet.union (ImmutableSet.ofList upwardsExposed)) liveOut

            result <-
                result
                |> ImmutableMap.add blockValue { LiveIn = liveIn; LiveOut = liveOut }

            liveIn)
           (fun b succ ->
               succ
               |> Seq.map (fun (bb, liveIn) ->
                   liveIn
                   |> Option.defaultValue ImmutableSet.empty
                   |> ImmutableSet.fold (fun result instr ->
                       match instr with
                       | Ref { Content = PhiInstruction { Incoming = list }
                               ParentBlock = Some block } when bb = block ->
                           list
                           |> List.tryFind (snd >> (=) b)
                           |> Option.filter (fst >> (!) >> Value.isInstruction)
                           |> Option.map
                               (fst
                                >> Seq.singleton
                                >> ImmutableSet
                                >> ImmutableSet.union result)
                           |> Option.defaultValue result
                       | instr -> result |> ImmutableSet.add instr) ImmutableSet.empty)
               |> ImmutableSet.unionMany)
    |> ignore

    result

let private reorderBasicBlocks _ irModule =

    !irModule
    |> Module.revBasicBlocks
    |> List.choose
        ((!)
         >> Value.asBasicBlock
         >> BasicBlock.tryTerminator
         >> Option.filter ((!) >> Value.isUnconditional >> not))
    |> List.iter (fun terminator ->
        let successors =
            !terminator |> Value.operands |> List.tail

        // Only handling diamonds for now
        let mergeCount =
            successors
            |> List.map
                ((!)
                 >> BasicBlock.successors
                 >> ImmutableSet.ofList)
            |> ImmutableSet.unionMany
            |> ImmutableSet.count

        if mergeCount <= 1
           && successors
              |> List.forall
                  ((!)
                   >> Value.asBasicBlock
                   >> BasicBlock.tryTerminator
                   >> Option.map ((!) >> Value.isUnconditional)
                   >> Option.defaultValue false)
           && (successors.[1] |> BasicBlock.index) > (successors.[0] |> BasicBlock.index) then
            irModule
            |> Module.swapBlocks successors.[0] successors.[1])

    irModule

type private CPLatticeValues =
    | Top
    | Constant of int16
    | Bottom

let private constantPropagation _ irModule =

    let mutable executableEdges = ImmutableMap.empty

    let addEdge fromEdge toEdge map =
        match map |> ImmutableMap.tryFind fromEdge with
        | None ->
            map
            |> ImmutableMap.add fromEdge (ImmutableSet.ofList [ toEdge ])
        | Some set ->
            map
            |> ImmutableMap.add fromEdge (set |> ImmutableSet.add toEdge)

    let rewrites =
        !irModule
        |> Module.entryBlock
        |> Option.map
            (Graphs.dataFlowAnalysis
                (fun replacements current ->
                    !current
                    |> Value.asBasicBlock
                    |> BasicBlock.nonPhiInstructions
                    |> Seq.fold (fun replacements instr ->
                        let operands =
                            !instr
                            |> Value.operands
                            |> List.map (function
                                | ConstOp c -> Constant c
                                | instr when !instr |> Value.producesValue -> replacements |> ImmutableMap.find instr
                                | _ -> Bottom)
                            |> Array.ofSeq

                        let result =
                            match instr, operands with
                            | BinOp And _, [| Constant c1; Constant c2 |] -> c1 &&& c2 |> Constant |> Some
                            | BinOp Add _, [| Constant c1; Constant c2 |] -> c1 + c2 |> Constant |> Some
                            | BinOp Or _, [| Constant c1; Constant c2 |] -> c1 ||| c2 |> Constant |> Some
                            | BinOp Xor _, [| Constant c1; Constant c2 |] -> c1 ^^^ c2 |> Constant |> Some
                            | BinOp Sub _, [| Constant c1; Constant c2 |] -> c1 - c2 |> Constant |> Some
                            | BinOp Mul _, [| Constant c1; Constant c2 |] -> c1 * c2 |> Constant |> Some
                            | BinOp SDiv _, [| Constant c1; Constant c2 |] -> c1 / c2 |> Constant |> Some
                            | BinOp SRem _, [| Constant c1; Constant c2 |] -> c1 % c2 |> Constant |> Some
                            | BinOp Shl _, [| Constant c1; Constant c2 |] -> c1 <<< (c2 |> int) |> Constant |> Some
                            | BinOp AShr _, [| Constant c1; Constant c2 |] -> c1 >>> (c2 |> int) |> Constant |> Some
                            | BinOp UDiv _, [| Constant c1; Constant c2 |] ->
                                (c1 |> uint16) / (c2 |> uint16)
                                |> int16
                                |> Constant
                                |> Some
                            | BinOp URem _, [| Constant c1; Constant c2 |] ->
                                (c1 |> uint16) % (c2 |> uint16)
                                |> int16
                                |> Constant
                                |> Some
                            | BinOp LShr _, [| Constant c1; Constant c2 |] ->
                                (c1 |> uint16) >>> (c2 |> int)
                                |> int16
                                |> Constant
                                |> Some
                            | UnaryOp Negate _, [| Constant c |] -> -c |> Constant |> Some
                            | UnaryOp Not _, [| Constant c |] -> ~~~c |> Constant |> Some
                            | CondBrOp (Zero, _, trueBranch, falseBranch), [| Constant c; _; _ |] ->
                                if c = 0s
                                then executableEdges <- executableEdges |> addEdge current trueBranch
                                else executableEdges <- executableEdges |> addEdge current falseBranch

                                None
                            | CondBrOp (Negative, _, trueBranch, falseBranch), [| Constant c; _; _ |] ->
                                if c < 0s
                                then executableEdges <- executableEdges |> addEdge current trueBranch
                                else executableEdges <- executableEdges |> addEdge current falseBranch

                                None
                            | CondBrOp (_, _, trueBranch, falseBranch), _ ->
                                executableEdges <- executableEdges |> addEdge current trueBranch
                                executableEdges <- executableEdges |> addEdge current falseBranch
                                None
                            | GotoOp destination, _ ->
                                executableEdges <- executableEdges |> addEdge current destination
                                None
                            | instr, _ when !instr |> Value.producesValue |> not -> None
                            | _, _ -> Some Bottom

                        match result with
                        | None -> replacements
                        | Some result -> replacements |> ImmutableMap.add instr result) replacements)
                 (fun current predecessors ->
                     predecessors
                     |> Seq.choose (fun (a, b) ->
                         match b with
                         | None -> None
                         | Some b -> Some(a, b))
                     |> Seq.map (fun (predecessor, replacement) ->
                         !current
                         |> Value.asBasicBlock
                         |> BasicBlock.phis
                         |> List.fold (fun replacement phi ->
                             let phiOp =
                                 !phi
                                 |> Value.operands
                                 |> List.pairwise
                                 |> List.find (snd >> (=) predecessor)
                                 |> fst

                             let phiRepl =
                                 match phiOp with
                                 | ConstOp c -> Constant c
                                 | instr when !instr |> Value.producesValue ->
                                     replacement
                                     |> ImmutableMap.tryFind phiOp
                                     |> Option.defaultValue Top
                                 | UndefOp -> Top
                                 | _ -> Bottom

                             replacement |> ImmutableMap.add phi phiRepl) replacement)
                     |> List.ofSeq
                     |> Some
                     |> Option.filter (List.isEmpty >> not)
                     |> Option.map
                         (List.reduce (fun map1 map2 ->
                             Seq.append map1 map2
                             |> Seq.groupBy (fun kv -> kv.Key)
                             |> Seq.map (fun (key, values) ->
                                 (key,
                                  values
                                  |> Seq.map (fun kv -> kv.Value)
                                  |> Seq.reduce (fun op1 op2 ->
                                      match op1, op2 with
                                      | Top, _ -> op2
                                      | _, Top -> op1
                                      | Constant c1, Constant c2 when c1 = c2 -> op1
                                      | _, _ -> Bottom)))
                             |> ImmutableMap.ofSeq))
                     |> Option.defaultValue ImmutableMap.empty)
                 ((!) >> BasicBlock.predecessors >> Seq.ofList)
                 (fun x ->
                     executableEdges
                     |> ImmutableMap.tryFind x
                     |> Option.defaultValue ImmutableSet.empty))
        |> Option.defaultValue ImmutableMap.empty

    !irModule
    |> Module.exitBlock
    |> Option.bind (fun x -> rewrites |> ImmutableMap.tryFind x)
    |> Option.defaultValue ImmutableMap.empty
    |> Seq.choose (fun kv ->
        let value, latticeValue = kv.Deconstruct()

        match latticeValue with
        | Constant c -> Some(value, c)
        | _ -> None)
    |> Seq.iter (fun (value, repl) ->
        value
        |> Value.replaceWith (Builder.createConstant repl))

    let builder = Builder.fromModule irModule

    !irModule
    |> Module.revBasicBlocks
    |> Seq.choose (fun bb ->
        match executableEdges |> ImmutableMap.tryFind bb with
        | Some successors when successors |> ImmutableSet.count = 1
                               && !bb |> BasicBlock.successors |> List.length > 1 -> Some(bb, successors |> Seq.head)
        | _ -> None)
    |> Seq.iter (fun (bb, succ) ->
        !bb
        |> Value.asBasicBlock
        |> BasicBlock.terminator
        |> Value.replaceWith (builder |> Builder.createGoto succ |> fst))

    let reachable =
        executableEdges
        |> Seq.map (fun kv -> kv.Value)
        |> ImmutableSet.unionMany
        |> ImmutableSet.union
            (!irModule
             |> Module.entryBlock
             |> Option.toList
             |> ImmutableSet.ofList)

    !irModule
    |> Module.revBasicBlocks
    |> Seq.filter (fun x -> reachable |> ImmutableSet.contains x |> not)
    |> Seq.iter Value.destroy

    irModule

let private analyzeBitsRead _ irModule =

    let addMask mask instr map =
        if !instr |> Value.producesValue |> not then
            map
        else
            match map |> ImmutableMap.tryFind instr with
            | None -> map |> ImmutableMap.add instr mask
            | Some pre -> map |> ImmutableMap.add instr (mask ||| pre)

    let nextPowerOf2 value =
        let value = value - 1us
        let value = value ||| (value >>> 1)
        let value = value ||| (value >>> 2)
        let value = value ||| (value >>> 4)
        let value = value ||| (value >>> 8)
        value + 1us


    !irModule
    |> Module.backwardAnalysis
        (fun unusedBits current ->

            !current
            |> Value.asBasicBlock
            |> BasicBlock.revInstructions
            |> List.fold (fun unusedBits instr ->
                let operands =
                    !instr
                    |> Value.operands
                    |> List.filter ((!) >> Value.isBasicBlock >> not)
                    |> Array.ofList

                let instrUsedBits =
                    match unusedBits |> ImmutableMap.tryFind instr with
                    | None -> 0xFFFFus
                    | Some instrUsedBits -> instrUsedBits

                match instr with
                | BinOp And (ConstOp c, _) ->
                    unusedBits
                    |> addMask (c |> uint16 &&& instrUsedBits) operands.[1]
                | BinOp And (_, ConstOp c) ->
                    unusedBits
                    |> addMask (c |> uint16 &&& instrUsedBits) operands.[0]
                | BinOp Or _
                | BinOp And _
                | BinOp Xor _ ->
                    unusedBits
                    |> addMask instrUsedBits operands.[0]
                    |> addMask instrUsedBits operands.[1]
                | BinOp SRem (ConstOp c, _) ->
                    let newMask =
                        (abs (c) |> uint16 |> nextPowerOf2) - 1us

                    let newMask = newMask ||| 0x8000us

                    unusedBits
                    |> addMask (instrUsedBits &&& newMask) operands.[0]
                | BinOp URem (ConstOp c, _) ->
                    let newMask = (c |> uint16 |> nextPowerOf2) - 1us

                    unusedBits
                    |> addMask (instrUsedBits &&& newMask) operands.[0]
                | BinOp SRem (_, ConstOp c) ->
                    let newMask =
                        (abs (c) |> uint16 |> nextPowerOf2) - 1us

                    let newMask = newMask ||| 0x8000us

                    unusedBits
                    |> addMask (instrUsedBits &&& newMask) operands.[0]
                | BinOp URem (_, ConstOp c) ->
                    let newMask = (c |> uint16 |> nextPowerOf2) - 1us

                    unusedBits
                    |> addMask (instrUsedBits &&& newMask) operands.[0]
                | BinOp SRem (_, _) ->
                    unusedBits
                    |> addMask 0xFFFFus operands.[0]
                    |> addMask 0xFFFFus operands.[1]
                | BinOp URem (_, _) ->
                    unusedBits
                    |> addMask 0xFFFFus operands.[0]
                    |> addMask 0xFFFFus operands.[1]
                | BinOp LShr (_, ConstOp c) ->
                    unusedBits
                    |> addMask (instrUsedBits <<< (c |> int)) operands.[0]
                | BinOp AShr (_, ConstOp c) ->
                    unusedBits
                    |> addMask (instrUsedBits <<< (c |> int)) operands.[0]
                | BinOp Shl (_, ConstOp c) ->
                    unusedBits
                    |> addMask (instrUsedBits >>> (c |> int)) operands.[0]
                | UnaryOp Negate _ -> unusedBits |> addMask 0xFFFFus operands.[0]
                | UnaryOp Not _ -> unusedBits |> addMask instrUsedBits operands.[0]
                | CondBrOp (Negative, _, _, _) -> unusedBits |> addMask 0x8000us operands.[0]
                | CondBrOp (Zero, _, _, _) -> unusedBits |> addMask 0xFFFFus operands.[0]
                | PhiOp _ ->
                    operands
                    |> Array.fold (fun unusedBits operand -> addMask instrUsedBits operand unusedBits) unusedBits
                | LoadOp _ -> unusedBits |> addMask 0xFFFFus operands.[0]
                | BinOp Mul (_, _)
                | BinOp Add (_, _)
                | BinOp Sub (_, _) ->

                    let newMask =
                        (instrUsedBits + 1us |> nextPowerOf2) - 1us

                    unusedBits
                    |> addMask newMask operands.[0]
                    |> addMask newMask operands.[1]
                | BinOp SDiv (_, _)
                | BinOp UDiv (_, _)
                | StoreOp _ ->
                    unusedBits
                    |> addMask 0xFFFFus operands.[0]
                    |> addMask 0xFFFFus operands.[1]
                | _ -> unusedBits) unusedBits)
           (fun _ successors ->
               successors
               |> Seq.choose snd
               |> List.ofSeq
               |> Some
               |> Option.filter (List.isEmpty >> not)
               |> Option.map
                   (List.reduce (fun map1 map2 ->
                       Seq.append map1 map2
                       |> Seq.groupBy (fun kv -> kv.Key)
                       |> Seq.map (fun (key, values) ->
                           (key,
                            values
                            |> Seq.map (fun kv -> kv.Value)
                            |> Seq.reduce (|||)))
                       |> ImmutableMap.ofSeq))
               |> Option.defaultValue ImmutableMap.empty)
    |> ImmutableMap.find (!irModule |> Module.entryBlock |> Option.get)

let analyzeBitsReadPass =
    { Pass = analyzeBitsRead
      DependsOn = [] }

let private deadBitElimination passManager irModule =

    let mutable executableEdges = ImmutableMap.empty

    let addEdge fromEdge toEdge map =
        match map |> ImmutableMap.tryFind fromEdge with
        | None ->
            map
            |> ImmutableMap.add fromEdge (ImmutableSet.ofList [ toEdge ])
        | Some set ->
            map
            |> ImmutableMap.add fromEdge (set |> ImmutableSet.add toEdge)

    let constantToValue c =
        Seq.unfold (fun current -> Some((current &&& 1us) <> 0us, current >>> 1)) (c |> uint16)
        |> Seq.take 16
        |> Seq.map Some
        |> List.ofSeq

    let adder initialCarry op1 op2 =
        List.fold2 (fun (result, carry) op1 op2 ->
            match carry, op1, op2 with
            | None, Some false, Some false
            | Some false, None, Some false
            | Some false, Some false, None -> (None :: result, Some false)
            | None, Some true, _
            | None, _, Some true
            | _, None, Some true
            | Some true, None, _
            | Some true, _, None
            | _, Some true, None
            | None, None, _
            | None, _, None
            | _, None, None -> (None :: result, None)
            | Some b1, Some b2, Some b3 ->
                (Some((b1 <> b2) <> b3) :: result, ((b1 && b2) || (b1 && b3) || (b2 && b3)) |> Some))
            (List.empty, initialCarry) op1 op2
        |> fst
        |> List.rev

    let addition = adder (Some false)

    let negate = List.map (Option.map not)

    let subtraction op1 op2 = adder (Some true) op1 (negate op2)

    let rewrites =
        !irModule
        |> Module.entryBlock
        |> Option.map
            (Graphs.dataFlowAnalysis
                (fun replacements current ->

                    !current
                    |> Value.asBasicBlock
                    |> BasicBlock.nonPhiInstructions
                    |> Seq.fold (fun replacements instr ->
                        let operands =
                            !instr
                            |> Value.operands
                            |> List.map (function
                                | ConstOp c -> constantToValue c
                                | instr when !instr |> Value.producesValue -> replacements |> ImmutableMap.find instr
                                | _ -> List.init 16 (fun _ -> None))
                            |> Array.ofSeq

                        let result =
                            match instr with
                            | BinOp And _ ->
                                List.map2 (fun op1 op2 ->
                                    match op1, op2 with
                                    | Some false, _
                                    | _, Some false -> Some false
                                    | None, _
                                    | _, None -> None
                                    | Some op1, Some op2 -> Some(op1 && op2)) operands.[0] operands.[1]
                                |> Some
                            | BinOp Or _ ->
                                List.map2 (fun op1 op2 ->
                                    match op1, op2 with
                                    | Some true, _
                                    | _, Some true -> Some true
                                    | None, _
                                    | _, None -> None
                                    | Some op1, Some op2 -> Some(op1 || op2)) operands.[0] operands.[1]
                                |> Some
                            | BinOp Xor _ ->
                                List.map2 (fun op1 op2 ->
                                    match op1, op2 with
                                    | None, _
                                    | _, None -> None
                                    | Some op1, Some op2 -> Some(op1 <> op2)) operands.[0] operands.[1]
                                |> Some
                            | BinOp Add _ -> addition operands.[0] operands.[1] |> Some
                            | BinOp Sub _ -> subtraction operands.[0] operands.[1] |> Some
                            | BinOp Mul _ -> List.init 16 (fun _ -> None) |> Some
                            | BinOp SDiv _ -> List.init 16 (fun _ -> None) |> Some
                            | BinOp SRem _ -> List.init 16 (fun _ -> None) |> Some
                            | BinOp Shl (_, ConstOp c) ->
                                operands.[0]
                                |> List.rev
                                |> List.skip (c |> int)
                                |> List.rev
                                |> List.append (List.init (c |> int) (fun _ -> Some false))
                                |> Some
                            | BinOp Shl _ -> List.init 16 (fun _ -> None) |> Some
                            | BinOp LShr (_, ConstOp c) ->
                                List.init (c |> int) (fun _ -> Some false)
                                |> List.append (operands.[0] |> List.skip (c |> int))
                                |> Some
                            | BinOp AShr (_, ConstOp c) ->
                                List.init (c |> int) (fun _ -> operands.[0].[15])
                                |> List.append (operands.[0] |> List.skip (c |> int))
                                |> Some
                            | BinOp AShr _ -> List.init 16 (fun _ -> None) |> Some
                            | BinOp UDiv _ -> List.init 16 (fun _ -> None) |> Some
                            | BinOp URem _ -> List.init 16 (fun _ -> None) |> Some
                            | BinOp LShr _ -> List.init 16 (fun _ -> None) |> Some
                            | UnaryOp Negate _ ->
                                operands.[0]
                                |> negate
                                |> addition (1s |> constantToValue)
                                |> Some
                            | UnaryOp Not _ -> operands.[0] |> negate |> Some
                            | CondBrOp (Zero, _, trueBranch, falseBranch) ->
                                if operands.[0] |> List.forall ((=) (Some false)) then
                                    executableEdges <- executableEdges |> addEdge current trueBranch
                                else if operands.[0] |> List.exists ((=) (Some true)) then
                                    executableEdges <- executableEdges |> addEdge current falseBranch
                                else
                                    executableEdges <- executableEdges |> addEdge current trueBranch
                                    executableEdges <- executableEdges |> addEdge current falseBranch

                                None
                            | CondBrOp (Negative, _, trueBranch, falseBranch) ->
                                match operands.[0].[15] with
                                | Some true -> executableEdges <- executableEdges |> addEdge current trueBranch
                                | Some false -> executableEdges <- executableEdges |> addEdge current falseBranch
                                | None ->
                                    executableEdges <- executableEdges |> addEdge current trueBranch
                                    executableEdges <- executableEdges |> addEdge current falseBranch

                                None
                            | GotoOp destination ->
                                executableEdges <- executableEdges |> addEdge current destination
                                None
                            | instr when !instr |> Value.producesValue |> not -> None
                            | _ -> List.init 16 (fun _ -> None) |> Some

                        match result with
                        | None -> replacements
                        | Some result -> replacements |> ImmutableMap.add instr result) replacements)
                 (fun current predecessors ->
                     predecessors
                     |> Seq.choose (fun (a, b) ->
                         match b with
                         | None -> None
                         | Some b -> Some(a, b))
                     |> Seq.map (fun (predecessor, replacement) ->
                         !current
                         |> Value.asBasicBlock
                         |> BasicBlock.phis
                         |> List.fold (fun replacement phi ->
                             let phiOp =
                                 !phi
                                 |> Value.operands
                                 |> List.pairwise
                                 |> List.find (snd >> (=) predecessor)
                                 |> fst

                             let phiRepl =
                                 match phiOp with
                                 | ConstOp c -> constantToValue c |> Some
                                 | instr when !instr |> Value.producesValue -> replacement |> ImmutableMap.tryFind phiOp
                                 | UndefOp -> constantToValue 0s |> Some
                                 | _ -> None

                             match phiRepl with
                             | Some phiRepl -> replacement |> ImmutableMap.add phi phiRepl
                             | None -> replacement

                             ) replacement)
                     |> List.ofSeq
                     |> Some
                     |> Option.filter (List.isEmpty >> not)
                     |> Option.map
                         (List.reduce (fun map1 map2 ->
                             Seq.append map1 map2
                             |> Seq.groupBy (fun kv -> kv.Key)
                             |> Seq.map (fun (key, values) ->
                                 (key,
                                  values
                                  |> Seq.map (fun kv -> kv.Value)
                                  |> Seq.reduce (List.map2 (fun c1 c2 -> if c1 = c2 then c1 else None))))
                             |> ImmutableMap.ofSeq))
                     |> Option.defaultValue ImmutableMap.empty)
                 ((!) >> BasicBlock.predecessors >> Seq.ofList)
                 (fun x ->
                     executableEdges
                     |> ImmutableMap.tryFind x
                     |> Option.defaultValue ImmutableSet.empty))
        |> Option.defaultValue ImmutableMap.empty

    let bitsRead =
        passManager
        |> PassManager.analysisData analyzeBitsReadPass

    !irModule
    |> Module.exitBlock
    |> Option.bind (fun x -> rewrites |> ImmutableMap.tryFind x)
    |> Option.defaultValue ImmutableMap.empty
    |> Seq.choose (fun kv ->
        let value, constants = kv.Deconstruct()

        let mask = bitsRead.[value]

        constants
        |> Seq.indexed
        |> Seq.fold (fun current (index, bit) ->
            match current, bit with
            | None, _ -> None
            | Some current, None -> if (mask &&& (1us <<< index)) = 0us then Some current else None
            | Some current, Some true -> (current <<< 1) ||| 1s |> Some
            | Some current, Some false -> (current <<< 1) |> Some) (Some 0s)
        |> Option.map (fun x -> (value, x)))
    |> Seq.iter (fun (value, repl) ->
        value
        |> Value.replaceWith (Builder.createConstant repl))

    let builder = Builder.fromModule irModule

    !irModule
    |> Module.revBasicBlocks
    |> Seq.choose (fun bb ->
        match executableEdges |> ImmutableMap.tryFind bb with
        | Some successors when successors |> ImmutableSet.count = 1
                               && !bb |> BasicBlock.successors |> List.length > 1 -> Some(bb, successors |> Seq.head)
        | _ -> None)
    |> Seq.iter (fun (bb, succ) ->
        !bb
        |> Value.asBasicBlock
        |> BasicBlock.terminator
        |> Value.replaceWith (builder |> Builder.createGoto succ |> fst))

    let reachable =
        executableEdges
        |> Seq.map (fun kv -> kv.Value)
        |> ImmutableSet.unionMany
        |> ImmutableSet.union
            (!irModule
             |> Module.entryBlock
             |> Option.toList
             |> ImmutableSet.ofList)

    !irModule
    |> Module.revBasicBlocks
    |> Seq.filter (fun x -> reachable |> ImmutableSet.contains x |> not)
    |> Seq.iter Value.destroy

    irModule

let analyzeLivenessPass =
    { Pass = analyzeLiveness
      DependsOn = [] }

let deadBitEliminationPass =
    { Pass = deadBitElimination
      DependsOn = [ analyzeBitsReadPass ]
      Invalidates = [] }

let instructionSimplifyPass =
    { Pass = instructionSimplify
      DependsOn = []
      Invalidates = [] }

let removeUnreachableBlocksPass =
    { Pass = removeUnreachableBlocks
      DependsOn = []
      Invalidates = [] }

let instructionCombinePass =
    { Pass = instructionCombine
      DependsOn = []
      Invalidates = [] }

let jumpThreadingPass =
    { Pass = jumpThreading
      DependsOn = []
      Invalidates =
          [ analyzeDominancePass
            analyzeDominanceFrontiersPass
            analyzeLivenessPass
            analyzeBitsReadPass ] }

let simplifyCFGPass =
    { Pass = simplifyCFG
      DependsOn = []
      Invalidates =
          [ analyzeDominancePass
            analyzeDominanceFrontiersPass ] }

let deadCodeEliminationPass =
    { Pass = deadCodeElimination
      DependsOn = []
      Invalidates = [] }

let mem2regPass =
    { Pass = mem2reg
      DependsOn =
          [ analyzeDominanceFrontiersPass
            analyzeAllocPass ]
      Invalidates =
          [ analyzeLivenessPass
            analyzeBitsReadPass ] }

let constantPropagationPass =
    { Pass = constantPropagation
      DependsOn = []
      Invalidates = [] }

let reorderBasicBlocksPass =
    { Pass = reorderBasicBlocks
      DependsOn = []
      Invalidates = [] }
