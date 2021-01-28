module Micro16C.MiddleEnd.Passes

open System.Collections.Generic
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util

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
    | BinOp And (ConstOp lhs, ConstOp rhs) ->
        value
        |> Value.replaceWith (Builder.createConstant (lhs &&& rhs))

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
    | BinOp Xor (ConstOp c1, ConstOp c2) ->
        value
        |> Value.replaceWith (Builder.createConstant (c1 ^^^ c2))

        true
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
    | BinOp Add (ConstOp lhs, ConstOp rhs) ->
        value
        |> Value.replaceWith (Builder.createConstant (lhs + rhs))

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
    | BinOp Mul (ConstOp c1, ConstOp c2) ->
        value
        |> Value.replaceWith (Builder.createConstant (c1 * c2))

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
    | BinOp SDiv (ConstOp c1, ConstOp c2) ->
        value
        |> Value.replaceWith (Builder.createConstant (c1 / c2))

        true
    | BinOp UDiv (ConstOp c1, ConstOp c2) ->
        value
        |> Value.replaceWith (Builder.createConstant (int16 ((c1 |> uint16) / (c2 |> uint16))))

        true
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
    | BinOp URem (ConstOp c1, ConstOp c2) ->
        value
        |> Value.replaceWith (Builder.createConstant (((c1 |> uint16) % (c2 |> uint16)) |> int16))

        true
    | BinOp SRem (ConstOp c1, ConstOp c2) ->
        value
        |> Value.replaceWith (Builder.createConstant (c1 % c2))

        true
    // (X % Y) % Y -> X % Y
    | BinOp SRem (BinOp SRem (_, y1) as op, y2) when y1 = y2 ->
        value |> Value.replaceWith op

        true
    | BinOp URem (BinOp URem (_, y1) as op, y2) when y1 = y2 ->
        value |> Value.replaceWith op

        true
    // Not patterns
    | UnaryOp Not (ConstOp c) ->
        value
        |> Value.replaceWith (Builder.createConstant (~~~c))

        true
    // ~(~X) -> X
    | UnaryOp Not (UnaryOp Not x) ->
        value |> Value.replaceWith x
        true
    // Negate patterns
    | UnaryOp Negate (ConstOp c) ->
        value
        |> Value.replaceWith (Builder.createConstant -c)

        true
    // -(-X) -> X
    | UnaryOp Negate (UnaryOp Negate x) ->
        value |> Value.replaceWith x
        true
    // Shl patterns
    | BinOp Shl (ConstOp lhs, ConstOp rhs) ->
        value
        |> Value.replaceWith (Builder.createConstant (lhs <<< (rhs |> int32)))

        true
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
    // lshr patterns
    | BinOp LShr (ConstOp lhs, ConstOp rhs) ->
        value
        |> Value.replaceWith (Builder.createConstant ((lhs |> uint16) >>> (rhs |> int32) |> int16))

        true
    // 0 >> X -> 0
    | BinOp LShr (ConstOp 0s, _) ->
        value
        |> Value.replaceWith (Builder.createConstant 0s)

        true
    // X >> 0 -> X
    | BinOp LShr (x, ConstOp 0s) ->
        value |> Value.replaceWith x

        true
    // ashr patterns
    | BinOp AShr (ConstOp lhs, ConstOp rhs) ->
        value
        |> Value.replaceWith (Builder.createConstant (lhs >>> (rhs |> int)))

        true
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
    | CondBrOp (Zero, ConstOp c, trueBranch, falseBranch) ->
        if c = 0s then
            value
            |> Value.replaceWith (builder |> Builder.createGoto trueBranch |> fst)
        else
            value
            |> Value.replaceWith (builder |> Builder.createGoto falseBranch |> fst)

        true
    | CondBrOp (Negative, ConstOp c, trueBranch, falseBranch) ->
        if c < 0s then
            value
            |> Value.replaceWith (builder |> Builder.createGoto trueBranch |> fst)
        else
            value
            |> Value.replaceWith (builder |> Builder.createGoto falseBranch |> fst)

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

let instructionSimplify (irModule: Module ref) =

    let builder = Builder.fromModule irModule

    while !irModule
          |> Module.instructions
          |> List.map (singleInstructionSimplify builder)
          |> List.exists id do
        ()

    irModule

let deadCodeElimination (irModule: Module ref) =

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

let simplifyCFG (irModule: Module ref) =

    let simplifyBlock blockValue =
        if !blockValue |> Value.isBasicBlock |> not then
            // As we may delete a successor this case could occur
            false
        else
            let block = !blockValue |> Value.asBasicBlock
            // this optimization may be invalid if the basic block is used in a Phi. For now I'll be conservative and
            // not remove such basic blocks. As a future TODO I could check for semantic changes
            match block |> BasicBlock.revInstructions with
            | [ Ref { Content = GotoInstruction { BasicBlock = destination } } ] when !blockValue
                                                                                      |> Value.users
                                                                                      |> List.exists (function
                                                                                          | Ref { Content = PhiInstruction _ } ->
                                                                                              true
                                                                                          | _ -> false)
                                                                                      |> not ->
                blockValue |> Value.replaceWith destination
                true
            | Ref { Content = GotoInstruction { BasicBlock = destination } } as terminator :: _ when (!destination
                                                                                                      |> BasicBlock.hasSinglePredecessor)
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

let removeUnreachableBlocks (irModule: Module ref) =

    let set =
        !irModule
        |> Module.preOrder
        |> Seq.fold (fun set bb -> set |> ImmutableSet.add bb) ImmutableSet.empty

    !irModule
    |> Module.revBasicBlocks
    |> Seq.filter (fun v -> ImmutableSet.contains v set |> not)
    |> Seq.iter Value.destroy

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

let jumpThreading irModule =

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
                    | Ref { Content = PhiInstruction _ } as phi ->
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
                     >> List.iter (singleInstructionSimplify builder >> ignore))

                newBlock
                |> Option.iter
                    ((!)
                     >> Value.asBasicBlock
                     >> BasicBlock.instructions
                     >> List.iter (singleInstructionCombine builder >> ignore))

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
                             | Ref { Content = GotoInstruction _ } -> false
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
                    let incomingList =
                        copies
                        |> List.map (fun (block, replacements) -> (replacements |> ImmutableMap.find oldValue, block))

                    let mergeIncoming =
                        !blockValue
                        |> BasicBlock.successors
                        |> List.map (fun bb ->
                            let phi =
                                builder
                                |> Builder.setInsertBlock (Some bb)
                                |> Builder.setInsertPoint Start
                                |> Builder.createPhi incomingList
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

let instructionCombine (irModule: Module ref) =

    let builder = Builder.fromModule irModule

    while !irModule
          |> Module.instructions
          |> List.map (singleInstructionCombine builder)
          |> List.exists id do
        ()

    irModule

let analyzeAlloc (irModule: Module ref) =
    let analyzeAlloc instr =
        match !instr with
        | { Content = AllocationInstruction ({ Aliased = None } as alloca)
            Users = users } ->
            let addressTaken =
                users
                |> List.exists (function
                    | Ref { Content = LoadInstruction _ } -> false
                    | Ref { Content = StoreInstruction { Value = value } } when value <> instr -> false
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
            |> List.iter Value.destroy

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
    |> Module.revBasicBlocks
    |> List.iter removeRedundantLoadStoresInBlock

    irModule

let analyzeDominance (irModule: Module ref) =
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
    |> processBlocks

    !irModule
    |> Module.revBasicBlocks
    |> List.iter (fun x ->
        let block = Value.asBasicBlock !x

        x
        := { !x with
                 Content = BasicBlockValue { block with ImmediatelyDominates = [] } })

    !irModule
    |> Module.revBasicBlocks
    |> List.iter (fun x ->
        let block = Value.asBasicBlock !x

        let iDom = map.[x]

        iDom
        |> Option.iter (fun ref ->
            let block = Value.asBasicBlock !ref

            ref
            := { !ref with
                     Content =
                         BasicBlockValue
                             { block with
                                   ImmediatelyDominates = x :: block.ImmediatelyDominates } })

        x
        := { !x with
                 Content = BasicBlockValue { block with ImmediateDominator = iDom } })

    irModule

let analyzeDominanceFrontiers (irModule: Module ref) =

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
                    map.[runner] <- map.[runner] |> ImmutableSet.add b

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
    |> Module.revBasicBlocks
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
                        match !x with
                        | { Content = StoreInstruction { Value = passThrough } } when loadStores
                                                                                      |> ImmutableSet.contains x ->
                            x |> Value.destroy
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
        |> Module.entryBlock
        |> Option.map (rename Value.UndefValue)
        |> ignore

        alloca |> Value.destroy)

    irModule

let analyzeLiveness irModule =

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
                        match !instr with
                        | { Content = PhiInstruction _ } -> (set, [ instr ])
                        | _ ->
                            let set =
                                if !instr |> Value.producesValue then set |> ImmutableSet.remove instr else set

                            (set,
                             !instr
                             |> Value.operands
                             |> List.filter ((!) >> Value.isInstruction))

                    set
                    |> ImmutableSet.union (ImmutableSet.ofList upwardsExposed)) liveOut

            blockValue
            := { !blockValue with
                     Content =
                         BasicBlockValue
                             { block with
                                   LiveIn = liveIn
                                   LiveOut = liveOut } }

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

    irModule

let reorderBasicBlocks irModule =

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
