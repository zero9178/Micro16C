module Micro16C.Backend.IR

open System


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

type Value =
    { mutable Uses: int
      Name: string
      Content: ValueContent }

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
    { Left: Value
      Kind: BinaryKind
      Right: Value }

and UnaryKind =
    | Not
    | Shl
    | Shr

and UnaryInstruction = { Kind: UnaryKind; Value: Value }

and LoadInstruction = { Source: Value }

and StoreInstruction = { Destination: Value; Value: Value }

and GotoInstruction = { BasicBlock: BasicBlock }

and CondBrInstruction =
    { Condition: Value
      TrueBranch: BasicBlock
      FalseBranch: BasicBlock }

and PhiInstruction = { Incoming: (Value * BasicBlock) list }

and BasicBlock =
    { Name: string
      Instructions: Value list }

type Module = { BasicBlocks: BasicBlock list }

type Builder =
    { BasicBlock: BasicBlock option
      ValueNames: Set<string>
      BasicBlockNames: Set<string> }

module Builder =

    let rec private uniqueName (name: string) (set: Set<string>) =
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
                      |> string)
                     + (1 + (numSuffix |> Array.ofList |> string |> int)
                        |> string))
                    set
        else
            name

    let valueName (name: string) (builder: Builder) =
        let name = uniqueName name builder.ValueNames

        (name,
         { builder with
               ValueNames = Set.add name builder.ValueNames })

    let blockName (name: string) (builder: Builder) =
        let name = uniqueName name builder.BasicBlockNames

        (name,
         { builder with
               BasicBlockNames = Set.add name builder.BasicBlockNames })

    let addValue (value: Value) (builder: Builder) =
        match builder.BasicBlock with
        | None -> builder
        | Some block ->
            { builder with
                  BasicBlock =
                      Some
                          { block with
                                Instructions = value :: block.Instructions } }

    let createBasicBlock (name: string) (builder: Builder) =
        let name, builder = blockName name builder
        ({ Name = name; Instructions = [] }, builder)

    let insertPoint (builder: Builder) = builder.BasicBlock

    let setInsertPoint (basicBlock: BasicBlock option) (builder: Builder) = { builder with BasicBlock = basicBlock }

    let createConstant (value: int16) =
        { Uses = 0
          Name = ""
          Content = Constant { Value = value } }

    let createRegisterNamedAlloca (register: Register option) (name: string) (builder: Builder) =
        let name, builder = valueName name builder

        let value =
            { Uses = 0
              Name = name
              Content = AllocationInstruction { Register = register } }

        let builder = builder |> addValue value
        (value, builder)

    let createNamedAlloca = createRegisterNamedAlloca None

    let createAlloca = createNamedAlloca ""

    let createNamedBinary (name: string) (left: Value) (kind: BinaryKind) (right: Value) (builder: Builder) =
        left.Uses <- left.Uses + 1
        right.Uses <- right.Uses + 1

        let name, builder = valueName name builder

        let value =
            { Uses = 0
              Name = name
              Content =
                  BinaryInstruction
                      { Left = left
                        Kind = kind
                        Right = right } }

        let builder = builder |> addValue value
        (value, builder)

    let createBinary = createNamedBinary ""

    let createNamedUnary (name: string) (kind: UnaryKind) (value: Value) (builder: Builder) =
        value.Uses <- value.Uses + 1

        let name, builder = valueName name builder

        let value =
            { Uses = 0
              Name = name
              Content = UnaryInstruction { Kind = kind; Value = value } }

        let builder = builder |> addValue value
        (value, builder)

    let createUnary = createNamedUnary ""

    let createNamedLoad (name: string) (value: Value) (builder: Builder) =
        value.Uses <- value.Uses + 1

        let name, builder = valueName name builder

        let value =
            { Uses = 0
              Name = name
              Content = LoadInstruction { Source = value } }

        let builder = builder |> addValue value
        (value, builder)

    let createLoad = createNamedLoad ""

    let createNamedStore (name: string) (destination: Value) (value: Value) (builder: Builder) =
        value.Uses <- value.Uses + 1

        let name, builder = valueName name builder

        let value =
            { Uses = 0
              Name = name
              Content =
                  StoreInstruction
                      { Destination = destination
                        Value = value } }

        let builder = builder |> addValue value
        (value, builder)

    let createStore = createNamedStore ""

    let createGoto (destination: BasicBlock) (builder: Builder) =
        let value =
            { Uses = 0
              Name = ""
              Content = GotoInstruction { BasicBlock = destination } }

        let builder = builder |> addValue value
        (value, builder)

    let createCondBr (condition: Value) (trueBranch: BasicBlock) (falseBranch: BasicBlock) (builder: Builder) =
        condition.Uses <- condition.Uses + 1

        let value =
            { Uses = 0
              Name = ""
              Content =
                  CondBrInstruction
                      { Condition = condition
                        TrueBranch = trueBranch
                        FalseBranch = falseBranch } }

        let builder = builder |> addValue value
        (value, builder)

    let createNamedPhi (name: string) (incoming: (Value * BasicBlock) list) (builder: Builder) =
        incoming
        |> List.map fst
        |> List.iter (fun value -> value.Uses <- value.Uses + 1)

        let name, builder = valueName name builder

        let value =
            { Uses = 0
              Name = name
              Content = PhiInstruction { Incoming = incoming } }

        (value, builder)

    let createPhi = createNamedPhi ""
