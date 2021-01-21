module Micro16C.MiddleEnd.IR

open System
open System.Collections.Generic
open Micro16C.MiddleEnd.Util

let (|Ref|) (ref: 'T ref) = ref.Value

type Register =
    | R0
    | R1
    | R2
    | R3
    | R4
    | R5
    | R6
    | R7
    | R8
    | R9
    | R10
    | AC
    | PC

    member this.asString =
        match this with
        | PC -> "PC"
        | R0 -> "R0"
        | R1 -> "R1"
        | R2 -> "R2"
        | R3 -> "R3"
        | R4 -> "R4"
        | R5 -> "R5"
        | R6 -> "R6"
        | R7 -> "R7"
        | R8 -> "R8"
        | R9 -> "R9"
        | R10 -> "R10"
        | AC -> "AC"


[<NoComparison>]
[<ReferenceEquality>]
[<StructuredFormatDisplay("{Name}")>]
type Value =
    { Users: Value ref list
      Name: string
      Content: ValueContent
      ParentBlock: Value ref option
      Register: Register option }

    override this.ToString() = this.Name

    static member Default =
        { Users = []
          Name = ""
          Content = Undef
          ParentBlock = None
          Register = None }

    static member UndefValue = ref Value.Default

and ValueContent =
    | Constant of Constant
    | Register of Register
    | AllocationInstruction of AllocationInstruction
    | BinaryInstruction of BinaryInstruction
    | UnaryInstruction of UnaryInstruction
    | LoadInstruction of LoadInstruction
    | StoreInstruction of StoreInstruction
    | GotoInstruction of GotoInstruction
    | CondBrInstruction of CondBrInstruction
    | PhiInstruction of PhiInstruction
    | BasicBlockValue of BasicBlock
    | Undef

and Constant = { Value: int16 }

and AllocationInstruction = { Aliased: bool option }

and BinaryKind =
    | And
    | Add
    | Or
    | Xor
    | Sub
    | Mul
    | SDiv
    | UDiv
    | SRem
    | URem

and BinaryInstruction =
    { Left: Value ref
      Kind: BinaryKind
      Right: Value ref }

and UnaryKind =
    | Not
    | Shl
    | Shr

and UnaryInstruction = { Kind: UnaryKind; Value: Value ref }

and LoadInstruction = { Source: Value ref }

and StoreInstruction =
    { Destination: Value ref
      Value: Value ref }

and GotoInstruction = { BasicBlock: Value ref }

and CondBrKind =
    | Zero
    | Negative

and CondBrInstruction =
    { Kind: CondBrKind
      Value: Value ref
      TrueBranch: Value ref
      FalseBranch: Value ref }

and PhiInstruction =
    { Incoming: (Value ref * Value ref) list
      ValuesMemory: ImmutableMap<Value ref, Value ref> }

and BasicBlock =
    { Instructions: Value ref list
      ImmediateDominator: Value ref option
      ImmediatelyDominates: Value ref list
      DominanceFrontier: Value ref list option
      ParentModule: Module ref
      LiveIn: ImmutableSet<Value ref>
      LiveOut: ImmutableSet<Value ref> }

and Module =
    internal
        { BasicBlocks: Value ref list }

        static member Default = { BasicBlocks = [] }

        override this.ToString() =
            let mutable counter = 0

            let mutable seenValues =
                Dictionary<Value, string>(HashIdentity.Reference)

            let seenNames = ref Set.empty

            this.BasicBlocks
            |> Seq.rev
            |> Seq.fold (fun text blockValue ->

                let getName (value: Value ref) =
                    match !value with
                    | { Content = Constant { Value = constant } } -> constant |> string
                    | { Content = Register register } -> register.asString
                    | { Content = Undef } -> "undef"
                    | { Name = name } ->
                        match seenValues.TryGetValue !value with
                        | (true, name) -> name
                        | (false, _) ->
                            match name with
                            | "" ->
                                counter <- counter + 1
                                let name = "%" + ((counter - 1) |> string)
                                seenValues.Add(!value, name)
                                name
                            | _ ->
                                let rec uniqueName name =
                                    if Set.contains name !seenNames then
                                        match name
                                              |> List.ofSeq
                                              |> List.rev
                                              |> List.takeWhile Char.IsDigit with
                                        | [] -> uniqueName (name + "0")
                                        | digits ->
                                            let newInt =
                                                digits
                                                |> List.rev
                                                |> List.toArray
                                                |> String
                                                |> int
                                                |> ((+) 1)

                                            let name =
                                                name
                                                |> List.ofSeq
                                                |> List.rev
                                                |> List.skip (List.length digits)
                                                |> List.rev
                                                |> List.toArray
                                                |> String

                                            uniqueName (name + (newInt |> string))
                                    else
                                        seenValues.Add(!value, name)
                                        seenNames := Set.add name !seenNames
                                        name

                                uniqueName ("%" + name)

                let block =
                    match (!blockValue).Content with
                    | BasicBlockValue block -> block
                    | _ -> failwith "Internal Compiler Error"

                let text =
                    text + sprintf "%s:\n" (getName blockValue)

                (block.Instructions
                 |> List.rev
                 |> List.fold (fun text instruction ->
                     match !instruction with
                     | { Content = AllocationInstruction _ } ->
                         text
                         + sprintf "\t%s = alloca\n" (getName instruction)
                     | { Content = GotoInstruction goto } ->
                         text
                         + sprintf "\tgoto %s\n" (getName goto.BasicBlock)
                     | { Content = BinaryInstruction binary } ->
                         let opName =
                             match binary.Kind with
                             | Add -> "add"
                             | And -> "and"
                             | Or -> "or"
                             | Xor -> "xor"
                             | Sub -> "sub"
                             | Mul -> "mul"
                             | SDiv -> "sdiv"
                             | UDiv -> "udiv"
                             | SRem -> "srem"
                             | URem -> "urem"

                         text
                         + sprintf "\t%s = %s %s %s\n" (getName instruction) opName (getName binary.Left)
                               (getName binary.Right)
                     | { Content = UnaryInstruction unary } ->
                         let opName =
                             match unary.Kind with
                             | Not -> "not"
                             | Shl -> "shl"
                             | Shr -> "shr"

                         text
                         + sprintf "\t%s = %s %s\n" (getName instruction) opName (getName unary.Value)
                     | { Content = LoadInstruction load } ->
                         text
                         + sprintf "\t%s = load %s\n" (getName instruction) (getName load.Source)
                     | { Content = CondBrInstruction cr } ->

                         let opName =
                             match cr.Kind with
                             | Negative -> "< 0"
                             | Zero -> "= 0"

                         text
                         + sprintf "\tbr %s %s %s %s\n" (getName cr.Value) opName (getName cr.TrueBranch)
                               (getName cr.FalseBranch)
                     | { Content = StoreInstruction store } ->
                         text
                         + sprintf "\tstore %s -> %s\n" (getName store.Value) (getName store.Destination)
                     | { Content = PhiInstruction phi } ->
                         let list =
                             phi.Incoming
                             |> List.map (fun (x, y) -> sprintf "(%s,%s)" (getName x) (getName y))
                             |> List.reduce (fun x y -> x + " " + y)

                         text
                         + sprintf "\t%s = phi %s\n" (getName instruction) list
                     | _ -> failwith "Internal Compiler Error") text)
                + "\n") ""

let rec private filterOnce predicate list =
    match list with
    | [] -> []
    | head :: list -> if not (predicate head) then list else head :: (filterOnce predicate list)

module internal BasicBlockInternal =

    let createDefault parent =
        { Instructions = []
          ImmediateDominator = None
          ImmediatelyDominates = []
          DominanceFrontier = None
          ParentModule = parent
          LiveIn = ImmutableSet.empty
          LiveOut = ImmutableSet.empty }

    let revInstructions basicBlock = basicBlock.Instructions

    let instructions = revInstructions >> List.rev

    let immediateDominator basicBlock = basicBlock.ImmediateDominator

    let dominanceFrontier basicBlock = basicBlock.DominanceFrontier

    let phis =
        instructions
        >> List.takeWhile (function
            | Ref { Content = PhiInstruction _ } -> true
            | _ -> false)

    let nonPhiInstructions =
        instructions
        >> List.skipWhile (function
            | Ref { Content = PhiInstruction _ } -> true
            | _ -> false)

    let strictDominators =
        Seq.unfold (fun blockValue ->
            !blockValue
            |> (function
            | { Content = BasicBlockValue block } -> block
            | _ -> failwith "")
            |> immediateDominator
            |> Option.filter ((=) blockValue)
            |> Option.map (fun x -> (x, x)))

    let dominators block =
        strictDominators block
        |> Seq.append (Seq.singleton block)

    let dominates other basicBlock =
        other |> dominators |> Seq.contains basicBlock

    let strictlyDominates other basicBlock =
        other
        |> strictDominators
        |> Seq.contains basicBlock

[<RequireQualifiedAccess>]
module Value =

    let name value = value.Name

    let parentBlock value = value.ParentBlock

    let users value = value.Users

    let register (value: Value) = value.Register

    let tracksUsers value =
        match value.Content with
        | Constant _
        | Register _
        | Undef -> false
        | _ -> true

    let isBasicBlock value =
        match value.Content with
        | BasicBlockValue _ -> true
        | _ -> false

    let useCount = users >> List.length

    let hasSideEffects value =
        match value.Content with
        | GotoInstruction _
        | StoreInstruction _
        | CondBrInstruction _ -> true
        | _ -> false

    let isInstruction value =
        match value.Content with
        | Constant _
        | Register _
        | Undef
        | BasicBlockValue _ -> false
        | _ -> true

    let producesValue value =
        match value.Content with
        | AllocationInstruction _
        | BinaryInstruction _
        | UnaryInstruction _
        | LoadInstruction _
        | PhiInstruction _ -> true
        | _ -> false

    let isTerminating value =
        match value.Content with
        | GotoInstruction _
        | CondBrInstruction _ -> true
        | _ -> false

    let isUnconditional value =
        match value.Content with
        | GotoInstruction _ -> true
        | _ -> false

    let asBasicBlock =
        function
        | { Content = BasicBlockValue value } -> value
        | _ -> failwith "Internal Compiler Error: Value is not a BasicBlock"

    let rec private addToPhis basicBlock successor =
        !successor
        |> asBasicBlock
        |> BasicBlockInternal.phis
        |> List.iter (fun phiValue ->
            match phiValue with
            | Ref { Content = PhiInstruction ({ Incoming = list } as phi) } ->
                if list |> List.exists (snd >> (=) basicBlock) |> not then
                    let value =
                        phi.ValuesMemory
                        |> ImmutableMap.tryFind basicBlock
                        |> Option.defaultValue Value.UndefValue

                    value |> addUser phiValue
                    basicBlock |> addUser phiValue

                    phiValue
                    := { !phiValue with
                             Content =
                                 PhiInstruction
                                     { phi with
                                           Incoming = (value, basicBlock) :: list } }
            | _ -> failwith "Internal Compiler Error")

    and internal addUser dependent operand =
        if !operand |> tracksUsers then
            operand
            := { !operand with
                     Users = dependent :: (!operand).Users }

            match !dependent |> parentBlock with
            | Some pred when !dependent |> isTerminating
                             && !operand |> isBasicBlock -> addToPhis pred operand
            | _ -> ()

    let rec private removeFromPhis basicBlock successor =
        !successor
        |> asBasicBlock
        |> BasicBlockInternal.phis
        |> List.iter (fun phiValue ->
            match phiValue with
            | Ref { Content = PhiInstruction ({ Incoming = list } as phi) } ->
                let trueList, falseList =
                    list |> List.partition (snd >> (<>) (basicBlock))

                phiValue
                := { !phiValue with
                         Content =
                             PhiInstruction
                                 { phi with
                                       Incoming = trueList
                                       ValuesMemory =
                                           falseList
                                           |> List.fold (fun map (value, bb) -> map |> ImmutableMap.add bb value)
                                                  phi.ValuesMemory } }

                falseList
                |> List.iter (fun (x, y) ->
                    x |> removeUser phiValue
                    y |> removeUser phiValue)
            | _ -> failwith "Internal Compiler Error")

    and private removeUser dependent operand =
        operand
        := { !operand with
                 Users = (!operand).Users |> filterOnce ((<>) dependent) }

        match !dependent |> parentBlock with
        | Some pred when !dependent |> isTerminating
                         && !operand |> isBasicBlock -> removeFromPhis pred operand
        | _ -> ()

    let private changeUser oldDependant operand dependent =
        if !operand |> tracksUsers then
            operand
            := { !operand with
                     Users = (dependent :: (!operand).Users) }

        oldDependant
        := { !oldDependant with
                 Users =
                     (!oldDependant).Users
                     |> filterOnce ((<>) dependent) }

    let operands value =
        match value.Content with
        | Constant _
        | Register _
        | BasicBlockValue _
        | Undef _
        | AllocationInstruction _ -> []
        | BinaryInstruction { Left = lhs; Right = rhs } -> [ lhs; rhs ]
        | GotoInstruction { BasicBlock = value }
        | UnaryInstruction { Value = value }
        | LoadInstruction { Source = value } -> [ value ]
        | StoreInstruction { Value = value
                             Destination = destination } -> [ destination; value ]
        | CondBrInstruction { Value = value
                              TrueBranch = trueBranch
                              FalseBranch = falseBranch } -> [ value; trueBranch; falseBranch ]
        | PhiInstruction { Incoming = list } ->
            list
            |> List.fold (fun state (x, y) -> y :: x :: state) []
            |> List.rev

    let setOperand index operand value =
        match (index, (!value).Content) with
        | (_, Constant _)
        | (_, Register _)
        | (_, AllocationInstruction _) -> failwith "Internal Compiler Error: Invalid Operand Index"
        | (i, UnaryInstruction _)
        | (i, GotoInstruction _)
        | (i, LoadInstruction _) when i >= 1 -> failwith "Internal Compiler Error: Invalid Operand Index"
        | (i, BinaryInstruction _)
        | (i, StoreInstruction _) when i >= 2 -> failwith "Internal Compiler Error: Invalid Operand Index"
        | (i, CondBrInstruction _) when i >= 3 -> failwith "Internal Compiler Error: Invalid Operand Index"
        | (i, PhiInstruction instr) when i >= 2 * List.length instr.Incoming ->
            failwith "Internal Compiler Error: Invalid Operand Index"
        | (0, CondBrInstruction instr) ->
            changeUser instr.Value operand value

            value
            := { !value with
                     Content = CondBrInstruction { instr with Value = operand } }
        | (1, CondBrInstruction instr) ->
            changeUser instr.TrueBranch operand value

            value
            := { !value with
                     Content = CondBrInstruction { instr with TrueBranch = operand } }
        | (2, CondBrInstruction instr) ->
            changeUser instr.FalseBranch operand value

            value
            := { !value with
                     Content = CondBrInstruction { instr with FalseBranch = operand } }
        | (0, UnaryInstruction instr) ->
            changeUser instr.Value operand value

            value
            := { !value with
                     Content = UnaryInstruction { instr with Value = operand } }
        | (0, GotoInstruction instr) ->
            changeUser instr.BasicBlock operand value

            value
            := { !value with
                     Content = GotoInstruction { instr with BasicBlock = operand } }
        | (0, LoadInstruction instr) ->
            changeUser instr.Source operand value

            value
            := { !value with
                     Content = LoadInstruction { instr with Source = operand } }
        | (0, BinaryInstruction instr) ->
            changeUser instr.Left operand value

            value
            := { !value with
                     Content = BinaryInstruction { instr with Left = operand } }
        | (1, BinaryInstruction instr) ->
            changeUser instr.Right operand value

            value
            := { !value with
                     Content = BinaryInstruction { instr with Right = operand } }
        | (0, StoreInstruction instr) ->
            changeUser instr.Destination operand value

            value
            := { !value with
                     Content = StoreInstruction { instr with Destination = operand } }
        | (1, StoreInstruction instr) ->
            changeUser instr.Value operand value

            value
            := { !value with
                     Content = StoreInstruction { instr with Value = operand } }
        | (i, PhiInstruction instr) ->

            value
            := { !value with
                     Content =
                         PhiInstruction
                             { instr with
                                   Incoming =
                                       instr.Incoming
                                       |> List.indexed
                                       |> List.map (fun pair ->
                                           match pair with
                                           | (j, (old, block)) when i / 2 = j && i % 2 = 0 ->
                                               changeUser old operand value
                                               (operand, block)
                                           | (j, (incoming, old)) when i / 2 = j && i % 2 = 1 ->
                                               changeUser old operand value
                                               (incoming, operand)
                                           | (_, x) -> x) } }

        | _ -> failwith "Internal Compiler Error"

    // Careful, in an instruction with multiple operands this will delete all occurrences
    let replaceOperand operand replacement value =
        !value
        |> operands
        |> List.indexed
        |> List.filter (snd >> ((=) operand))
        |> List.map fst
        |> List.iter (fun i -> setOperand i replacement value)

    let removeFromParent value =
        match (!value).ParentBlock with
        | None -> ()
        | Some (Ref { Content = BasicBlockValue block } as bb) ->
            bb
            := { !bb with
                     Content =
                         BasicBlockValue
                             { block with
                                   Instructions = block.Instructions |> List.filter ((<>) value) } }

            value := { !value with ParentBlock = None }
        | _ -> failwith "Internal Compiler Error"

    let rec destroy value =

        // If we are removing a basic block we need to destroy all it's instructions as well
        match (!value) with
        | { Content = BasicBlockValue ({ ParentModule = irModule } as block) } ->
            block.Instructions |> List.iter destroy

            irModule
            := { !irModule with
                     BasicBlocks =
                         (!irModule).BasicBlocks
                         |> List.filter ((<>) value) }
        | _ ->
            (!value)
            |> operands
            |> List.iter (removeUser value)

            value |> removeFromParent

        value := !Value.UndefValue

    and replaceWith replacement value =

        (!value).Users
        |> List.iter (replaceOperand value replacement)

        assert (useCount !value = 0)

        match (!replacement).ParentBlock with
        | None when isInstruction !replacement ->
            match (!value).ParentBlock with
            | None -> ()
            | Some parentBlockValue ->
                replacement
                := { !replacement with
                         ParentBlock = Some parentBlockValue }

                let index =
                    (!parentBlockValue |> asBasicBlock).Instructions
                    |> List.findIndex ((=) value)

                destroy value

                let parentBlock = !parentBlockValue |> asBasicBlock

                let (first, second) =
                    parentBlock.Instructions |> List.splitAt index

                parentBlockValue
                := { !parentBlockValue with
                         Content =
                             BasicBlockValue
                                 { parentBlock with
                                       Instructions = first @ [ replacement ] @ second } }

                if !replacement |> isTerminating then
                    !replacement
                    |> operands
                    |> List.filter ((!) >> isBasicBlock)
                    |> List.iter (addToPhis parentBlockValue)
        | _ -> destroy value

    let dominates other value =
        if !value
           |> parentBlock
           |> Option.get
           |> BasicBlockInternal.dominates other
           |> not then
            false
        else if (!value |> parentBlock) <> (!other |> parentBlock) then
            true
        else
            !value
            |> parentBlock
            |> Option.get
            |> (!)
            |> asBasicBlock
            |> BasicBlockInternal.revInstructions
            |> List.find (fun bb -> bb = value || bb = other) = value

    let strictlyDominates other value =
        if other = value then false else dominates other value

[<RequireQualifiedAccess>]
module BasicBlock =

    let createDefault = BasicBlockInternal.createDefault

    let revInstructions = BasicBlockInternal.revInstructions

    let instructions = BasicBlockInternal.instructions

    let immediateDominator = BasicBlockInternal.immediateDominator

    let dominanceFrontier = BasicBlockInternal.dominanceFrontier

    let index block =
        let bb = !block |> Value.asBasicBlock

        (List.length (!bb.ParentModule).BasicBlocks)
        - ((!bb.ParentModule).BasicBlocks
           |> List.findIndex ((=) block))
        - 1

    let liveIn block = block.LiveIn

    let liveOut block = block.LiveOut

    let immediatelyDominates block = block.ImmediatelyDominates

    let tryTerminator =
        revInstructions
        >> List.tryHead
        >> Option.filter ((!) >> Value.isTerminating)

    let terminator = tryTerminator >> Option.get

    let successors =
        Value.asBasicBlock
        >> tryTerminator
        >> Option.map
            ((!)
             >> Value.operands
             >> List.filter ((!) >> Value.isBasicBlock))
        >> Option.defaultValue []

    let predecessors =
        Value.users
        >> List.filter ((!) >> Value.isTerminating)
        >> List.choose ((!) >> Value.parentBlock)

    let dominators = BasicBlockInternal.dominators

    let dominates = BasicBlockInternal.dominates

    let strictDominators = BasicBlockInternal.strictDominators

    let strictlyDominates = BasicBlockInternal.strictlyDominates

    let hasSingleSuccessor =
        successors >> Seq.tryExactlyOne >> Option.isSome

    let hasSinglePredecessor =
        predecessors >> Seq.tryExactlyOne >> Option.isSome

    let hasNoPredecessor = predecessors >> Seq.isEmpty

    let phis = BasicBlockInternal.phis

    let nonPhiInstructions = BasicBlockInternal.nonPhiInstructions

[<RequireQualifiedAccess>]
module Module =

    let basicBlocks irModule = irModule.BasicBlocks |> List.rev

    let revBasicBlocks irModule = irModule.BasicBlocks

    let entryBlock = revBasicBlocks >> List.tryLast

    let exitBlock = revBasicBlocks >> List.tryHead

    let swapBlocks block1 block2 irModule =
        assert (Value.isBasicBlock !block1)
        assert (Value.isBasicBlock !block2)

        irModule
        := { !irModule with
                 BasicBlocks =
                     (!irModule).BasicBlocks
                     |> List.map (fun bb ->
                         if bb = block1 then block2
                         else if bb = block2 then block1
                         else bb) }

    let revInstructions =
        revBasicBlocks
        >> List.map
            ((!)
             >> Value.asBasicBlock
             >> BasicBlock.revInstructions)
        >> List.concat

    let instructions = revInstructions >> List.rev

    let asText (irModule: Module) = irModule.ToString()

    let preOrder =
        entryBlock
        >> Option.map (Graphs.preOrder ((!) >> BasicBlock.successors >> Seq.ofList))
        >> Option.defaultValue Seq.empty

    let postOrder =
        entryBlock
        >> Option.map (Graphs.postOrder ((!) >> BasicBlock.successors >> Seq.ofList))
        >> Option.defaultValue Seq.empty

    let reversePostOrder =
        entryBlock
        >> Option.map (Graphs.reversePostOrder ((!) >> BasicBlock.successors >> Seq.ofList))
        >> Option.defaultValue Seq.empty

    let forwardAnalysis transform join =
        entryBlock
        >> Option.iter
            (Graphs.singleForwardAnalysis
                transform
                 join
                 ((!) >> BasicBlock.predecessors >> Seq.ofList)
                 ((!) >> BasicBlock.successors >> Seq.ofList))

    let backwardAnalysis transform join =
        entryBlock
        >> Option.iter (Graphs.backwardAnalysis transform join ((!) >> BasicBlock.successors >> Seq.ofList))

type InsertPoint =
    | Before of Value ref
    | After of Value ref
    | Start
    | End

type Builder =

    private
        { InsertBlock: Value ref option
          InsertIndex: int
          Module: Module ref
          NotYetInserted: ImmutableMap<Value ref, InsertPoint> }



[<RequireQualifiedAccess>]
module Builder =

    let fromModule irModule =
        { InsertBlock = None
          InsertIndex = 0
          Module = irModule
          NotYetInserted = ImmutableMap.empty }

    let private isBasicBlockOrUndef =
        function
        | { Content = BasicBlockValue _ } -> true
        | { Content = Undef _ } -> true
        | _ -> false

    let private addValue value builder =
        match builder.InsertBlock with
        | None -> (value, builder)
        | Some (Ref { Content = BasicBlockValue block } as blockVal) ->

            let (first, second) =
                block.Instructions
                |> List.splitAt builder.InsertIndex

            blockVal
            := { !blockVal with
                     Content =
                         BasicBlockValue
                             { block with
                                   Instructions = first @ [ value ] @ second } }

            value
            := { !value with
                     ParentBlock = Some blockVal }

            (value, builder)
        | _ -> failwith "Internal Compiler Error"

    let insertValue value builder =
        if !value |> Value.isInstruction then
            value |> Value.removeFromParent
            builder |> addValue value
        else
            (value, builder)

    let createBasicBlockAt (insertPoint: InsertPoint) name builder =
        let basicBlock =
            ref
                { Value.Default with
                      Name = name
                      Content = BasicBlockValue(BasicBlock.createDefault builder.Module) }

        (basicBlock,
         { builder with
               NotYetInserted =
                   builder.NotYetInserted
                   |> (ImmutableMap.add basicBlock insertPoint) })

    let createBasicBlock = createBasicBlockAt End

    let insertBlock builder = builder.InsertBlock

    let afterInstr builder =
        match builder.InsertBlock with
        | None -> failwith "Internal Compiler Error: No current insert block"
        | Some blockValue ->
            let block = !blockValue |> Value.asBasicBlock

            if builder.InsertIndex = List.length block.Instructions then
                Start
            else
                block.Instructions
                |> List.item builder.InsertIndex
                |> After

    let beforeInstr builder =
        match builder.InsertBlock with
        | None -> failwith "Internal Compiler Error: No current insert block"
        | Some blockValue ->
            let block = !blockValue |> Value.asBasicBlock

            if builder.InsertIndex = 0 then
                End
            else
                (block.Instructions
                 |> List.item (builder.InsertIndex - 1))
                |> Before

    let setInsertBlock basicBlock builder =
        assert (basicBlock
                |> Option.map ((!) >> isBasicBlockOrUndef)
                |> Option.defaultValue true)

        let builder =
            match basicBlock
                  |> Option.map (fun block ->
                      builder.NotYetInserted
                      |> ImmutableMap.tryFind block) with
            | None
            | Some None -> builder
            | Some (Some insertPoint) ->
                let basicBlock = basicBlock |> Option.get

                match insertPoint with
                | End ->
                    builder.Module
                    := { !builder.Module with
                             BasicBlocks = basicBlock :: (!builder.Module).BasicBlocks }
                | Start ->
                    builder.Module
                    := { !builder.Module with
                             BasicBlocks = (!builder.Module).BasicBlocks @ [ basicBlock ] }
                | After ref ->
                    match (!builder.Module).BasicBlocks
                          |> List.tryFindIndex ((=) ref) with
                    | None -> failwith "Internal Compiler Error: Failed to find Basic Block in block list"
                    | Some i ->
                        let (first, second) =
                            (!builder.Module).BasicBlocks |> List.splitAt i

                        builder.Module
                        := { !builder.Module with
                                 BasicBlocks = first @ [ basicBlock ] @ second }
                | Before ref ->
                    match (!builder.Module).BasicBlocks
                          |> List.tryFindIndex ((=) ref) with
                    | None -> failwith "Internal Compiler Error: Failed to find Basic Block in block list"
                    | Some i ->
                        let (first, second) =
                            (!builder.Module).BasicBlocks
                            |> List.splitAt (i + 1)

                        builder.Module
                        := { BasicBlocks = first @ [ basicBlock ] @ second }

                { builder with
                      NotYetInserted =
                          builder.NotYetInserted
                          |> ImmutableMap.remove basicBlock }

        { builder with
              InsertBlock = basicBlock
              InsertIndex = 0 }

    let setInsertPoint (insertPoint: InsertPoint) builder =
        match builder.InsertBlock with
        | None -> builder
        | Some blockValue ->
            let block = !blockValue |> Value.asBasicBlock

            match insertPoint with
            | End -> { builder with InsertIndex = 0 }
            | Start ->
                { builder with
                      InsertIndex = List.length block.Instructions }
            | After ref ->
                match block.Instructions |> List.tryFindIndex ((=) ref) with
                | None -> failwith "Internal Compiler Error: Failed to find After instruction in insert block"
                | Some i -> { builder with InsertIndex = i }
            | Before ref ->
                match block.Instructions |> List.tryFindIndex ((=) ref) with
                | None -> failwith "Internal Compiler Error: Failed to find Before instruction in insert block"
                | Some i -> { builder with InsertIndex = i + 1 }

    let private constantCache = ref Map.empty<int16, Value ref>

    let createConstant value =
        match !constantCache |> Map.tryFind value with
        | None ->
            let ref =
                ref
                    { Value.Default with
                          Name = value |> string
                          Content = Constant { Value = value } }

            constantCache
            := !constantCache |> Map.add value ref

            ref
        | Some ref -> ref

    let createRegister register =
        ref
            { Value.Default with
                  Content = Register register }

    let createNamedAlloca name builder =
        let value =
            ref
                { Value.Default with
                      Name = name
                      Content = AllocationInstruction { Aliased = None } }

        builder |> addValue value

    let createAlloca = createNamedAlloca ""

    let createNamedBinary name left kind right builder =
        let value =
            ref
                { Value.Default with
                      Name = name
                      Content =
                          BinaryInstruction
                              { Left = left
                                Kind = kind
                                Right = right } }

        left |> Value.addUser value
        right |> Value.addUser value

        builder |> addValue value

    let createBinary = createNamedBinary ""

    let createNamedUnary name kind value builder =
        let unary =
            ref
                { Value.Default with
                      Name = name
                      Content = UnaryInstruction { Kind = kind; Value = value } }

        value |> Value.addUser unary

        builder |> addValue unary

    let createUnary = createNamedUnary ""

    let createNamedLoad name value builder =

        let load =
            ref
                { Value.Default with
                      Name = name
                      Content = LoadInstruction { Source = value } }

        value |> Value.addUser load

        builder |> addValue load

    let createLoad = createNamedLoad ""

    let createStore destination value builder =

        let store =
            ref
                { Value.Default with
                      Content =
                          StoreInstruction
                              { Destination = destination
                                Value = value } }

        value |> Value.addUser store
        destination |> Value.addUser store

        builder |> addValue store

    let createGoto destination builder =

        assert (!destination |> isBasicBlockOrUndef)

        let value =
            ref
                { Value.Default with
                      Content = GotoInstruction { BasicBlock = destination } }

        builder |> addValue value |> ignore

        destination |> Value.addUser value
        (value, builder)

    let createCondBr kind condition trueBranch falseBranch builder =

        assert (!trueBranch |> isBasicBlockOrUndef)
        assert (!falseBranch |> isBasicBlockOrUndef)

        let value =
            ref
                { Value.Default with
                      Content =
                          CondBrInstruction
                              { Kind = kind
                                Value = condition
                                TrueBranch = trueBranch
                                FalseBranch = falseBranch } }

        builder |> addValue value |> ignore

        condition |> Value.addUser value
        trueBranch |> Value.addUser value
        falseBranch |> Value.addUser value

        (value, builder)

    let createNamedPhi name incoming builder =

        let value =
            ref
                { Value.Default with
                      Name = name
                      Content =
                          PhiInstruction
                              { Incoming = incoming
                                ValuesMemory = ImmutableMap.empty } }

        incoming
        |> List.map (snd >> (!) >> isBasicBlockOrUndef)
        |> List.iter (fun x -> assert x)

        incoming
        |> List.iter (fun (x, y) ->
            x |> Value.addUser value
            y |> Value.addUser value)

        builder |> addValue value

    let createPhi = createNamedPhi ""

    let copy value operands builder =
        if !value |> Value.isInstruction |> not then
            assert (operands |> List.isEmpty)
            (value, builder)
        else
            match (!value).Content with
            | AllocationInstruction _ -> builder |> createAlloca
            | BinaryInstruction { Kind = kind } ->
                assert (operands |> List.length = 2)

                builder
                |> createBinary operands.[0] kind operands.[1]
            | UnaryInstruction { Kind = kind } ->
                assert (operands |> List.length = 1)
                builder |> createUnary kind operands.[0]
            | LoadInstruction _ ->
                assert (operands |> List.length = 1)
                builder |> createLoad operands.[0]
            | StoreInstruction _ ->
                assert (operands |> List.length = 2)
                builder |> createStore operands.[0] operands.[1]
            | GotoInstruction _ ->
                assert (operands |> List.length = 1)
                builder |> createGoto operands.[0]
            | CondBrInstruction { Kind = kind } ->
                assert (operands |> List.length = 3)

                builder
                |> createCondBr kind operands.[0] operands.[1] operands.[2]
            | PhiInstruction _ ->
                assert ((operands |> List.length) % 2 = 0)
                builder |> createPhi (operands |> List.pairwise)
            | _ -> failwith "Internal Compiler Error"

    let copyInstructionsStructure replacements instructions builder =
        let structure =
            instructions
            |> List.fold (fun structure instr ->
                match replacements |> ImmutableMap.tryFind instr with
                | Some repl -> structure |> ImmutableMap.add instr repl
                | None ->
                    let result =
                        builder
                        |> copy
                            instr
                               (!instr
                                |> Value.operands
                                |> List.map (fun op ->
                                    match structure |> ImmutableMap.tryFind op with
                                    | Some op -> op
                                    | None -> op))
                        |> fst

                    match (!result, !instr) with
                    | { ParentBlock = Some parentBlock }, { ParentBlock = Some oldParent } when !result
                                                                                                |> Value.isTerminating ->
                        !result
                        |> Value.operands
                        |> List.filter ((!) >> Value.isBasicBlock)
                        |> List.iter (fun succ ->
                            !succ
                            |> Value.asBasicBlock
                            |> BasicBlock.phis
                            |> List.iter (fun phi ->
                                let oldValue =
                                    !phi
                                    |> Value.operands
                                    |> List.pairwise
                                    |> List.find (snd >> (=) oldParent)
                                    |> fst

                                let index =
                                    (!phi
                                     |> Value.operands
                                     |> List.findIndex ((=) parentBlock))
                                    - 1

                                match structure |> ImmutableMap.tryFind oldValue with
                                | None -> phi |> Value.setOperand index oldValue
                                | Some repl -> phi |> Value.setOperand index repl))
                    | _ -> ()


                    structure |> ImmutableMap.add instr result) ImmutableMap.empty

        structure

    let splitBlockAt (insertPoint: InsertPoint) builder =

        let i, instructions, block =
            match insertPoint with
            | Start
            | End -> failwith "Can't split a basic block at it's start or beginning"
            | Before instr ->
                let block =
                    !instr |> Value.parentBlock |> Option.get

                let instructions =
                    !block
                    |> Value.asBasicBlock
                    |> BasicBlock.revInstructions

                (instructions |> List.findIndex ((=) instr), instructions, block)
            | After instr ->
                let block =
                    !instr |> Value.parentBlock |> Option.get

                let instructions =
                    !block
                    |> Value.asBasicBlock
                    |> BasicBlock.revInstructions

                ((instructions |> List.findIndex ((=) instr)) - 1, instructions, block)

        let instr = List.splitAt (i + 1) instructions |> snd

        let newBlock, builder =
            builder |> createBasicBlockAt (Before block) ""

        let builder =
            builder |> setInsertBlock (Some newBlock)

        let builder =
            instr
            |> Seq.fold (fun builder value -> insertValue value builder |> snd) builder
            |> createGoto block
            |> snd

        !block
        |> Value.users
        |> Seq.filter ((!) >> Value.isTerminating)
        |> Seq.iter (Value.replaceOperand block newBlock)

        (newBlock, builder)
