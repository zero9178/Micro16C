module Micro16C.MiddleEnd.IR

open System

let (|Ref|) (ref: 'T ref) = ref.Value

type Register =
    | PC
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
[<NoEquality>]
type Value =
    { Users: Value ref list
      Name: string
      Content: ValueContent
      ParentBlock: Value ref option }

    static member Default =
        { Users = []
          Name = ""
          Content = Constant { Value = 0s }
          ParentBlock = None }

and ValueContent =
    | Constant of Constant
    | AllocationInstruction of AllocationInstruction
    | BinaryInstruction of BinaryInstruction
    | UnaryInstruction of UnaryInstruction
    | LoadInstruction of LoadInstruction
    | StoreInstruction of StoreInstruction
    | GotoInstruction of GotoInstruction
    | CondBrInstruction of CondBrInstruction
    | PhiInstruction of PhiInstruction
    | BasicBlockValue of BasicBlock

and Constant = { Value: int16 }

and AllocationInstruction =
    { Register: Register option
      Aliased: bool option }

and BinaryKind =
    | And
    | Add

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

and CondBrInstruction =
    { Condition: Value ref
      TrueBranch: Value ref
      FalseBranch: Value ref }

and PhiInstruction =
    { Incoming: (Value ref * Value ref) list }

and BasicBlock =
    { Instructions: Value ref list
      Predecessors: Value ref list
      Successors: Value ref list }

    static member Default =
        { Instructions = []
          Predecessors = []
          Successors = [] }

module Value =

    let addUser dependent operand =
        operand
        := { !operand with
                 Users = dependent :: (!operand).Users }

    let operands value =
        match value.Content with
        | Constant _
        | BasicBlockValue _
        | AllocationInstruction _ -> []
        | BinaryInstruction { Left = lhs; Right = rhs } -> [ lhs; rhs ]
        | GotoInstruction { BasicBlock = value }
        | UnaryInstruction { Value = value }
        | LoadInstruction { Source = value } -> [ value ]
        | StoreInstruction { Value = value
                             Destination = destination } -> [ value; destination ]
        | CondBrInstruction { Condition = value
                              TrueBranch = trueBranch
                              FalseBranch = falseBranch } -> [ value; trueBranch; falseBranch ]
        | PhiInstruction { Incoming = list } ->
            list
            |> List.fold (fun state (x, y) -> y :: x :: state) []
            |> List.rev

    let withOperand index operand value =
        match (index, value.Content) with
        | (_, Constant _)
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
            { value with
                  Content = CondBrInstruction { instr with Condition = operand } }
        | (1, CondBrInstruction instr) ->
            { value with
                  Content = CondBrInstruction { instr with TrueBranch = operand } }
        | (2, CondBrInstruction instr) ->
            { value with
                  Content = CondBrInstruction { instr with FalseBranch = operand } }
        | (0, UnaryInstruction instr) ->
            { value with
                  Content = UnaryInstruction { instr with Value = operand } }
        | (0, GotoInstruction instr) ->
            { value with
                  Content = GotoInstruction { instr with BasicBlock = operand } }
        | (0, LoadInstruction instr) ->
            { value with
                  Content = LoadInstruction { instr with Source = operand } }
        | (0, BinaryInstruction instr) ->
            { value with
                  Content = BinaryInstruction { instr with Left = operand } }
        | (1, BinaryInstruction instr) ->
            { value with
                  Content = BinaryInstruction { instr with Right = operand } }
        | (0, StoreInstruction instr) ->
            { value with
                  Content = StoreInstruction { instr with Value = operand } }
        | (1, StoreInstruction instr) ->
            { value with
                  Content = StoreInstruction { instr with Destination = operand } }
        | (i, PhiInstruction instr) ->
            { value with
                  Content =
                      PhiInstruction
                          { instr with
                                Incoming =
                                    instr.Incoming
                                    |> List.indexed
                                    |> List.map (fun pair ->
                                        match pair with
                                        | (j, (_, block)) when i / 2 = j && i % 2 = 0 -> (operand, block)
                                        | (j, (value, _)) when i / 2 = j && i % 2 = 1 -> (value, operand)
                                        | (_, x) -> x) } }

        | _ -> failwith "Internal Compiler Error"

    let useCount value = value.Users |> List.length

    let hasSideEffects value =
        match value.Content with
        | GotoInstruction _
        | StoreInstruction _
        | CondBrInstruction _ -> true
        | _ -> false

    let isInstruction value =
        match value.Content with
        | Constant _
        | BasicBlockValue _ -> false
        | _ -> true

    let eraseFromParent value =
        match (!value).ParentBlock with
        | None -> ()
        | Some (Ref { Content = BasicBlockValue block } as bb) ->
            bb
            := { !bb with
                     Content =
                         BasicBlockValue
                             { block with
                                   Instructions =
                                       block.Instructions
                                       |> List.filter (fun x -> (!x).Name <> (!value).Name) } }
        | _ -> failwith "Internal Compiler Error"

    let replaceWith replacement value =

        !value
        |> operands
        |> List.iter (fun operand ->
            operand
            := { !operand with
                     Users =
                         (!operand).Users
                         |> List.filter (fun x -> (!x).Name <> (!value).Name) })

        (!value).Users
        |> List.iter (fun user ->
            let operandIndices =
                !user
                |> operands
                |> List.indexed
                |> List.filter (fun x -> (!(snd x)).Name = (!value).Name)
                |> List.map fst

            operandIndices
            |> List.iter (fun i ->
                addUser user replacement
                user := withOperand i replacement !user))

        match (!value, !replacement) with
        | ({ Content = BasicBlockValue valueBlock }, { Content = BasicBlockValue replacementBlock }) ->
            valueBlock.Successors
            |> List.iter (fun succ ->
                match !succ with
                | { Content = BasicBlockValue succBlock } ->
                    succ
                    := { !succ with
                             Content =
                                 BasicBlockValue
                                     { succBlock with
                                           Predecessors =
                                               (succBlock.Predecessors
                                                |> List.filter (fun x -> (!x).Name <> (!value).Name))
                                               @ valueBlock.Predecessors } }
                | _ -> failwith "Internal Compiler Error")

            valueBlock.Predecessors
            |> List.iter (fun pred ->
                match !pred with
                | { Content = BasicBlockValue predBlock } ->
                    pred
                    := { !pred with
                             Content =
                                 BasicBlockValue
                                     { predBlock with
                                           Successors =
                                               replacement
                                               :: (predBlock.Successors
                                                   |> List.filter (fun x -> (!x).Name <> (!value).Name)) } }
                | _ -> failwith "Internal Compiler Error")

        | ({ Content = BasicBlockValue _ }, _)
        | (_, { Content = BasicBlockValue _ }) -> failwith "Internal Compiler Error"
        | _ -> ()

        match (!replacement).ParentBlock with
        | None when isInstruction !replacement ->
            value
            := { !replacement with
                     ParentBlock = (!value).ParentBlock }
        | _ -> eraseFromParent value

    let asBasicBlock value =
        match value with
        | { Content = BasicBlockValue value } -> value
        | _ -> failwith "Internal Compiler Error: Value is not a BasicBlock"

module BasicBlock =

    let addEdge from toValue =
        let fromBlock = !from |> Value.asBasicBlock
        let toBlock = !toValue |> Value.asBasicBlock

        from
        := { !from with
                 Content =
                     BasicBlockValue
                         { fromBlock with
                               Successors = toValue :: fromBlock.Successors } }

        toValue
        := { !toValue with
                 Content =
                     BasicBlockValue
                         { toBlock with
                               Predecessors = from :: toBlock.Predecessors } }


[<NoComparison>]
[<NoEquality>]
type Module =
    { BasicBlocks: Value ref list }
    override this.ToString() =
        this.BasicBlocks
        |> List.fold (fun text blockValue ->
            let block = !blockValue |> Value.asBasicBlock

            let pred =
                block.Predecessors
                |> List.map (fun x -> (!x).Name)

            let succ =
                block.Successors |> List.map (fun x -> (!x).Name)

            let text =
                text + sprintf "; succ = %A pred = %A\n" succ pred

            let text =
                text + sprintf "%s:\n" (!blockValue).Name

            (block.Instructions
             |> List.fold (fun text instruction ->
                 match !instruction with
                 | { Content = AllocationInstruction { Register = None } } ->
                     text
                     + sprintf "\t%s = alloca\n" (!instruction).Name
                 | { Content = AllocationInstruction { Register = Some reg } } ->
                     text
                     + sprintf "\t%s = alloca (%s)\n" (!instruction).Name reg.asString
                 | { Content = GotoInstruction goto } ->
                     text
                     + sprintf "\tgoto %s\n" (!goto.BasicBlock).Name
                 | { Content = BinaryInstruction binary } ->
                     let opName =
                         match binary.Kind with
                         | Add -> "add"
                         | And -> "and"

                     text
                     + sprintf "\t%s = %s %s %s\n" (!instruction).Name opName (!binary.Left).Name (!binary.Right).Name
                 | { Content = UnaryInstruction unary } ->
                     let opName =
                         match unary.Kind with
                         | Not -> "not"
                         | Shl -> "shl"
                         | Shr -> "shr"

                     text
                     + sprintf "\t%s = %s %s\n" (!instruction).Name opName (!unary.Value).Name
                 | { Content = LoadInstruction load } ->
                     text
                     + sprintf "\t%s = load %s\n" (!instruction).Name (!load.Source).Name
                 | { Content = CondBrInstruction cr } ->
                     text
                     + sprintf "\tbr %s %s %s\n" (!cr.Condition).Name (!cr.TrueBranch).Name (!cr.FalseBranch).Name
                 | { Content = StoreInstruction store } ->
                     text
                     + sprintf "\tstore %s -> %s\n" (!store.Value).Name (!store.Destination).Name
                 | { Content = PhiInstruction phi } ->
                     let list =
                         phi.Incoming
                         |> List.map (fun (x, y) -> sprintf "(%s,%s)" (!x).Name (!y).Name)
                         |> List.reduce (fun x y -> x + " " + y)

                     text
                     + sprintf "\t%s = phi %s\n" (!instruction).Name list
                 | _ -> failwith "Internal Compiler Error") text)
            + "\n") ""

module Module =

    let instructions irModule =
        irModule.BasicBlocks
        |> Seq.ofList
        |> Seq.map ((!) >> Value.asBasicBlock)
        |> Seq.map (fun { Instructions = instr } -> instr)
        |> Seq.concat

    let basicBlocks irModule = irModule.BasicBlocks

type Builder =
    { InsertBlock: Value ref option
      ValueNames: Set<string>
      BasicBlockNames: Set<string>
      AllBlocks: Value ref list
      AlreadyInsertedBlocks: Set<string> }

    static member Default =
        { InsertBlock = None
          ValueNames = Set([ "" ])
          BasicBlockNames = Set([ "" ])
          AllBlocks = []
          AlreadyInsertedBlocks = Set([]) }

module Builder =

    let rec private uniqueName name (set: Set<string>) =
        if Set.contains name set then
            let numSuffix =
                name
                |> List.ofSeq
                |> List.rev
                |> List.takeWhile Char.IsDigit
                |> List.rev

            match numSuffix with
            | [] -> uniqueName (name + "0") set
            | _ ->
                uniqueName
                    ((name
                      |> List.ofSeq
                      |> List.rev
                      |> List.skip (List.length numSuffix)
                      |> List.rev
                      |> Array.ofList
                      |> String)
                     + (1 + (numSuffix |> Array.ofList |> String |> int)
                        |> string))
                    set
        else
            name

    let private valueName name builder =
        let name = uniqueName name builder.ValueNames

        ("%" + name,
         { builder with
               ValueNames = Set.add name builder.ValueNames })

    let private blockName name builder =
        let name = uniqueName name builder.BasicBlockNames

        ("." + name,
         { builder with
               BasicBlockNames = Set.add name builder.BasicBlockNames })

    let private addValue value builder =
        match builder.InsertBlock with
        | None -> (value, builder)
        | Some (Ref { Content = BasicBlockValue block } as blockVal) ->
            blockVal
            := { !blockVal with
                     Content =
                         BasicBlockValue
                             { block with
                                   Instructions = value :: block.Instructions } }

            value
            := { !value with
                     ParentBlock = Some blockVal }

            (value, builder)
        | _ -> failwith "Internal Compiler Error"

    let createBasicBlock name builder =
        let name, builder = blockName name builder

        (ref
            { Value.Default with
                  Name = name
                  Content = BasicBlockValue BasicBlock.Default },
         builder)

    let insertPoint builder = builder.InsertBlock

    let setInsertPoint basicBlock builder =
        let builder =
            match basicBlock with
            | None -> builder
            | Some basicBlock ->
                if Set.contains (!basicBlock).Name builder.AlreadyInsertedBlocks then
                    builder
                else
                    { builder with
                          AlreadyInsertedBlocks = Set.add (!basicBlock).Name builder.AlreadyInsertedBlocks
                          AllBlocks = basicBlock :: builder.AllBlocks }

        { builder with
              InsertBlock = basicBlock }

    let createConstant value =
        ref
            { Value.Default with
                  Name = value |> string
                  Content = Constant { Value = value } }

    let createRegisterNamedAlloca register name builder =
        let name, builder = valueName name builder

        let value =
            ref
                { Value.Default with
                      Name = name
                      Content = AllocationInstruction { Register = register; Aliased = None } }

        builder |> addValue value

    let createNamedAlloca = createRegisterNamedAlloca None

    let createAlloca = createNamedAlloca ""

    let createNamedBinary name left kind right builder =
        match (!left, !right) with
        | ({ Content = Constant { Value = lhs } }, { Content = Constant { Value = rhs } }) ->
            match kind with
            | Add -> (createConstant (lhs + rhs), builder)
            | And -> (createConstant (lhs &&& rhs), builder)
        | _ ->
            let name, builder = valueName name builder

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
        match !value with
        | { Content = Constant { Value = value } } ->
            match kind with
            | Not -> (createConstant ~~~value, builder)
            | Shl -> (createConstant (value <<< 1), builder)
            // Micro16 does logical, not arithmetic right shifts
            | Shr -> (createConstant ((value |> uint16) >>> 1 |> int16), builder)
        | _ ->

            let name, builder = valueName name builder

            let unary =
                ref
                    { Value.Default with
                          Name = name
                          Content = UnaryInstruction { Kind = kind; Value = value } }

            value |> Value.addUser unary

            builder |> addValue unary

    let createUnary = createNamedUnary ""

    let createNamedLoad name value builder =

        let name, builder = valueName name builder

        let load =
            ref
                { Value.Default with
                      Name = name
                      Content = LoadInstruction { Source = value } }

        value |> Value.addUser load

        builder |> addValue load

    let createLoad = createNamedLoad ""

    let createStore destination value builder =

        let name, builder = valueName ".store" builder

        let store =
            ref
                { Value.Default with
                      Name = name
                      Content =
                          StoreInstruction
                              { Destination = destination
                                Value = value } }

        value |> Value.addUser store
        destination |> Value.addUser store

        builder |> addValue store |> snd

    let createGoto destination builder =

        let name, builder = valueName ".goto" builder

        builder.InsertBlock
        |> Option.iter (fun x -> BasicBlock.addEdge x destination)

        let value =
            ref
                { Value.Default with
                      Name = name
                      Content = GotoInstruction { BasicBlock = destination } }

        destination |> Value.addUser value

        builder |> addValue value |> snd

    let createCondBr condition trueBranch falseBranch builder =
        match !condition with
        | { Content = Constant { Value = condition } } when condition <> 0s -> createGoto trueBranch builder
        | { Content = Constant { Value = condition } } when condition = 0s -> createGoto falseBranch builder
        | _ ->

            let name, builder = valueName ".condBr" builder

            builder.InsertBlock
            |> Option.iter (fun x -> BasicBlock.addEdge x trueBranch)

            builder.InsertBlock
            |> Option.iter (fun x -> BasicBlock.addEdge x falseBranch)

            let value =
                ref
                    { Value.Default with
                          Name = name
                          Content =
                              CondBrInstruction
                                  { Condition = condition
                                    TrueBranch = trueBranch
                                    FalseBranch = falseBranch } }

            condition |> Value.addUser value
            trueBranch |> Value.addUser value
            falseBranch |> Value.addUser value

            builder |> addValue value |> snd

    let createNamedPhi name incoming builder =

        let name, builder = valueName name builder

        let value =
            ref
                { Value.Default with
                      Name = name
                      Content = PhiInstruction { Incoming = incoming } }

        incoming
        |> List.iter (fun (x, y) ->
            x |> Value.addUser value
            y |> Value.addUser value)

        builder |> addValue value

    let createPhi = createNamedPhi ""

    let finalize builder =
        // throughout any operations we always used prepend on lists as Lists are single linked and using prepend
        // would be O(n). If I were to have n appends we'd have a complexity of O(n^2). Using prepends and reversing
        // at the very end yields O(2n) = O(n) complexity. We don't want it in reverse order for pattern matching in passes
        let blocks =
            builder.AllBlocks
            |> List.map (fun x ->
                match !x with
                | { Content = BasicBlockValue block } ->
                    x
                    := { !x with
                             Content =
                                 BasicBlockValue
                                     { block with
                                           Instructions = block.Instructions |> List.rev } }
                | _ -> ()

                x)
            |> List.rev

        { BasicBlocks = blocks }
