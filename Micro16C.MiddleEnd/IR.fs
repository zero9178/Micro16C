module Micro16C.MiddleEnd.IR

open System


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

type Value =
    { Users: Value ref list
      Name: string
      Content: ValueContent }

    static member Default =
        { Users = []
          Name = ""
          Content = Constant { Value = 0s } }

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

and Constant = { Value: int16 }

and AllocationInstruction = { Register: Register option }

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

and GotoInstruction = { BasicBlock: BasicBlock ref }

and CondBrInstruction =
    { Condition: Value ref
      TrueBranch: BasicBlock ref
      FalseBranch: BasicBlock ref }

and PhiInstruction =
    { Incoming: (Value ref * BasicBlock ref) list }

and BasicBlock =
    { Name: string
      Instructions: Value ref list
      Predecessors: BasicBlock ref list
      Successors: BasicBlock ref list }

    static member Default =
        { Name = ""
          Instructions = []
          Predecessors = []
          Successors = [] }

module BasicBlock =

    let addEdge from toBlock =
        from
        := { !from with
                 Successors = toBlock :: (!from).Successors }

        toBlock
        := { !toBlock with
                 Predecessors = from :: (!toBlock).Predecessors }

module Value =

    let addUser dependent operand =
        operand
        := { !operand with
                 Users = dependent :: (!operand).Users }

    let operands value =
        match value.Content with
        | Constant _
        | AllocationInstruction _
        | GotoInstruction _ -> []
        | BinaryInstruction { Left = lhs; Right = rhs } -> [ lhs; rhs ]
        | UnaryInstruction { Value = value }
        | LoadInstruction { Source = value }
        | CondBrInstruction { Condition = value } -> [ value ]
        | StoreInstruction { Value = value
                             Destination = destination } -> [ value; destination ]
        | PhiInstruction { Incoming = list } -> list |> List.map fst

    let hasSideEffects value =
        match value.Content with
        | GotoInstruction _
        | StoreInstruction _
        | CondBrInstruction _ -> true
        | _ -> false


type Module =
    { BasicBlocks: BasicBlock ref list }
    override this.ToString() =
        this.BasicBlocks
        |> List.fold (fun text block ->
            let text = text + sprintf "%s:\n" (!block).Name

            ((!block).Instructions
             |> List.fold (fun text instruction ->
                 match !instruction with
                 | { Content = AllocationInstruction _ } ->
                     text
                     + sprintf "\t%s = alloca\n" (!instruction).Name
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

type Builder =
    { InsertBlock: BasicBlock ref option
      ValueNames: Set<string>
      BasicBlockNames: Set<string>
      AllBlocks: BasicBlock ref list
      AlreadyInsertedBlocks: Set<BasicBlock ref> }

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
        | Some block ->
            block
            := { !block with
                     Instructions = value :: (!block).Instructions }

            (value, builder)

    let createBasicBlock name builder =
        let name, builder = blockName name builder
        (ref { BasicBlock.Default with Name = name }, builder)

    let insertPoint builder = builder.InsertBlock

    let setInsertPoint basicBlock builder =
        let builder =
            match basicBlock with
            | None -> builder
            | Some basicBlock ->
                if Set.contains basicBlock builder.AlreadyInsertedBlocks then
                    builder
                else
                    { builder with
                          AlreadyInsertedBlocks = Set.add basicBlock builder.AlreadyInsertedBlocks
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
                      Content = AllocationInstruction { Register = register } }

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

        let store =
            ref
                { Value.Default with
                      Name = ""
                      Content =
                          StoreInstruction
                              { Destination = destination
                                Value = value } }

        value |> Value.addUser store

        builder |> addValue store |> snd

    let createGoto destination builder =

        builder.InsertBlock
        |> Option.iter (fun x -> BasicBlock.addEdge x destination)

        let value =
            ref
                { Value.Default with
                      Name = ""
                      Content = GotoInstruction { BasicBlock = destination } }

        builder |> addValue value |> snd

    let createCondBr condition trueBranch falseBranch builder =
        match !condition with
        | { Content = Constant { Value = condition } } when condition <> 0s -> createGoto trueBranch builder
        | { Content = Constant { Value = condition } } when condition = 0s -> createGoto falseBranch builder
        | _ ->

            builder.InsertBlock
            |> Option.iter (fun x -> BasicBlock.addEdge x trueBranch)

            builder.InsertBlock
            |> Option.iter (fun x -> BasicBlock.addEdge x falseBranch)

            let value =
                ref
                    { Value.Default with
                          Name = ""
                          Content =
                              CondBrInstruction
                                  { Condition = condition
                                    TrueBranch = trueBranch
                                    FalseBranch = falseBranch } }

            condition |> Value.addUser value

            builder |> addValue value |> snd

    let createNamedPhi name incoming builder =

        let name, builder = valueName name builder

        let value =
            ref
                { Value.Default with
                      Name = name
                      Content = PhiInstruction { Incoming = incoming } }

        incoming
        |> List.map fst
        |> List.iter ((<|) (Value.addUser value))

        builder |> addValue value

    let createPhi = createNamedPhi ""

    let finalize builder =
        // throughout any operations we always used prepend on lists as Lists are single linked and using prepend
        // would be O(n). If I were to have n appends we'd have a complexity of O(n^2). Using prepends and reversing
        // at the very end yields O(2n) = O(n) complexity. We don't want it in reverse order for pattern matching in passes
        let blocks =
            builder.AllBlocks
            |> List.map (fun x ->
                x
                := { !x with
                         Instructions =
                             (!x).Instructions
                             |> List.map (fun x ->
                                 x
                                 := { !x with
                                          Users = (!x).Users |> List.rev }

                                 x)
                             |> List.rev }

                x)
            |> List.rev

        { BasicBlocks = blocks }
