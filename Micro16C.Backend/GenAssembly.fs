module Micro16C.Backend.GenAssembly

open System
open Micro16C.Backend.Assembly
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util

let private operandToBus operand =
    match !operand with
    | { Content = Constant { Value = 0s } } -> Bus.Zero |> Some
    | { Content = Constant { Value = 1s } } -> Bus.One |> Some
    | { Content = Constant { Value = -1s } } -> Bus.NegOne |> Some
    | { Content = Register reg } -> Register.toBus reg |> Some
    | _ ->
        !operand
        |> Value.register
        |> Option.map Register.toBus

let private prependOperation operation list =
    match (operation, list) with
    | { Shifter = Some Shifter.Left
        ALU = Some ALU.ABus
        SBus = Some result
        ABus = Some aBus
        AMux = Some AMux.ABus } as op1,
      Operation ({ Shifter = Some Shifter.Left
                   ALU = Some ALU.ABus
                   SBus = Some sBus
                   ABus = Some input
                   BBus = None
                   AMux = Some AMux.ABus
                   Condition = None } as op2) :: rest when sBus = aBus
                                                           && Operation.canCombine
                                                               { op1 with
                                                                     Shifter = None
                                                                     ALU = None
                                                                     ABus = None
                                                                     SBus = None
                                                                     AMux = None }
                                                                  { op2 with
                                                                        Shifter = None
                                                                        ALU = None
                                                                        ABus = None
                                                                        SBus = None
                                                                        AMux = None } ->
        let op =
            Operation.combine
                { op1 with
                      Shifter = None
                      ALU = None
                      ABus = None
                      SBus = None }
                { op2 with
                      Shifter = None
                      ALU = None
                      ABus = None
                      SBus = None }

        Operation
            { op with
                  Shifter = Some Shifter.Left
                  AMux = Some AMux.ABus
                  ABus = Some input
                  BBus = Some input
                  ALU = Some ALU.Add
                  SBus = Some result }
        :: rest
    | { ALU = Some ALU.ABus
        ABus = Some aBus
        AMux = Some AMux.ABus
        SBus = None
        Condition = Some _ } as op1,
      Operation ({ Shifter = (None
                   | Some Shifter.Noop)
                   SBus = Some sBus } as op2) :: rest when aBus = sBus
                                                           && Operation.canCombine
                                                               { op1 with ALU = None; ABus = None }
                                                                  op2 ->
        let op =
            Operation.combine { op1 with ALU = None; ABus = None } op2

        Operation(op) :: rest
    | { ALU = Some ALU.ABus
        AMux = Some AMux.MBR
        SBus = None
        Condition = Some _ } as op1,
      Operation ({ Shifter = (None
                   | Some Shifter.Noop)
                   MBRWrite = Some true } as op2) :: rest when Operation.canCombine
                                                                   { op1 with ALU = None; ABus = None }
                                                                   op2 ->
        let op =
            Operation.combine { op1 with ALU = None; ABus = None } op2

        Operation(op) :: rest
    | op1, Operation op2 :: list when Operation.canCombine op1 op2 -> Operation(Operation.combine op1 op2) :: list
    | operation, list -> (Operation operation) :: list

let private prependMove fromReg toReg list =
    if fromReg = toReg then
        list
    else
        prependOperation
            { Operation.Default with
                  SBus = toReg |> Some
                  ABus = fromReg |> Some
                  AMux = Some AMux.ABus
                  ALU = Some ALU.ABus
                  Shifter = Some Shifter.Noop }
            list

let private genPhiMoves block list =

    // Following assumptions need to be true here:
    // Any block whose successor(s) contain a phi ends with an unconditional branch.
    // Therefore any such block also only has a single successor. If it does not have a single successor,
    // the successor definitely does not contain phis. This facts is guaranteed by the breakPhiCriticalEdges pass
    // run shortly before register allocation passes

    !block
    |> BasicBlock.successors
    |> Seq.tryExactlyOne
    |> Option.map ((!) >> Value.asBasicBlock >> BasicBlock.phis)
    |> Option.map (fun phis ->
        let transfers =
            phis
            |> Seq.fold (fun list phi ->
                match !phi
                      |> Value.operands
                      |> Seq.pairwise
                      |> Seq.find (snd >> (=) block)
                      |> fst with
                | Ref { Content = Undef } -> list
                | source ->
                    (source |> operandToBus |> Option.get, phi |> operandToBus |> Option.get)
                    :: list) []

        let assigned = Array.create 13 false

        !block
        |> Value.asBasicBlock
        |> BasicBlock.liveOut
        |> Seq.map
            ((!)
             >> Value.register
             >> Option.get
             >> RegisterAllocator.registerToIndex)
        |> Seq.iter (fun i -> Array.set assigned i true)

        let notZeroOutDegrees =
            transfers
            |> Seq.filter (fun (x, y) -> x <> y)
            |> Seq.countBy fst
            |> Seq.map fst
            |> Set

        let mutable remapped = Map.empty

        let transfers, list =
            transfers
            |> List.fold (fun (transfers, list) (fromReg, toReg) ->
                if toReg = fromReg
                   || notZeroOutDegrees |> Set.contains toReg then
                    ((remapped
                      |> Map.tryFind fromReg
                      |> Option.defaultValue fromReg,
                      toReg)
                     :: transfers,
                     list)
                else
                    let list = list |> prependMove fromReg toReg

                    remapped <- remapped |> Map.add fromReg toReg

                    fromReg
                    |> Bus.toRegister
                    |> Option.iter (fun reg -> Array.set assigned (reg |> RegisterAllocator.registerToIndex) false)

                    let transfers =
                        transfers
                        |> List.map (fun (a, b) -> if a = fromReg then (toReg, b) else (a, b))

                    (transfers, list)) ([], list)

        let maps =
            transfers
            |> List.filter (fun (x, y) -> x <> y)
            |> List.map (fun (x, y) -> (y, x))
            |> Map

        let temp =
            lazy
                (match assigned |> Array.tryFindIndex not with
                 | None ->
                     failwith "Too much register pressure to implement phi operation. Spilling is not yet implemented"
                 | Some i ->
                     Array.set assigned i true

                     i
                     |> RegisterAllocator.indexToRegister
                     |> Register.toBus)

        Seq.unfold (fun (maps, list) ->
            if maps |> Map.isEmpty then
                None
            else
                let brokenOpen = maps |> Seq.head
                let maps = maps |> Map.remove brokenOpen.Key

                let maps, list =
                    Seq.unfold (fun (current, toReg, maps, list) ->
                        match current with
                        | None -> None
                        | Some current ->
                            let list = prependMove current toReg list

                            match maps |> Map.tryFind current with
                            | None -> Some((maps, list), (None, current, maps, list))
                            | Some next ->
                                let maps = maps |> Map.remove current
                                Some((maps, list), (Some next, current, maps, list)))
                        (Some brokenOpen.Value, temp.Force(), maps, list)
                    |> Seq.last

                let list =
                    prependMove (temp.Force()) brokenOpen.Key list

                Some(list, (maps, list))) (maps, list)
        |> Seq.tryLast
        |> Option.defaultValue list)
    |> Option.defaultValue list

let genAssembly irModule: AssemblyLine list =

    let mutable counter = 0

    let mutable seenValues = ImmutableMap.empty

    let seenNames = ref Set.empty

    let getName (value: Value ref) =
        match seenValues |> ImmutableMap.tryFind value with
        | Some name -> name
        | None ->
            match !value |> Value.name with
            | "" ->
                counter <- counter + 1
                let name = (counter - 1) |> string
                seenValues <- seenValues |> ImmutableMap.add value name
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
                        seenValues <- seenValues |> ImmutableMap.add value name
                        seenNames := Set.add name !seenNames
                        name

                uniqueName (!value |> Value.name)

    let basicBlocks =
        !irModule |> Module.basicBlocks |> Array.ofList

    basicBlocks
    |> Array.indexed
    |> Array.fold (fun list (bbIndex, bbValue) ->
        let bb = !bbValue |> Value.asBasicBlock

        let list = (bbValue |> getName |> Label) :: list

        bb
        |> BasicBlock.instructions
        |> List.fold (fun list instr ->

            let list =
                if Value.isTerminating !instr
                then genPhiMoves (!instr |> Value.parentBlock |> Option.get) list
                else list

            match !instr with
            | { Content = LoadInstruction { Source = Ref { Content = Register _ } as op } } ->
                prependMove (operandToBus op |> Option.get) (operandToBus instr |> Option.get) list
            | { Content = StoreInstruction { Destination = Ref { Content = Register _ } as instr
                                             Value = op } } ->
                prependMove (operandToBus op |> Option.get) (operandToBus instr |> Option.get) list
            | { Content = BinaryInstruction { Left = lhs; Right = rhs; Kind = kind } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus
                          ABus = lhs |> operandToBus
                          BBus = rhs |> operandToBus
                          ALU = Some(if kind = Add then ALU.Add else ALU.And)
                          Shifter = Some Shifter.Noop }
                    list
            | { Content = UnaryInstruction { Value = value; Kind = Not } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus
                          ABus = value |> operandToBus
                          ALU = Some ALU.Neg
                          Shifter = Some Shifter.Noop }
                    list
            | { Content = UnaryInstruction { Value = value; Kind = Shl } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus
                          ABus = value |> operandToBus
                          ALU = Some ALU.ABus
                          Shifter = Some Shifter.Left }
                    list
            | { Content = UnaryInstruction { Value = value; Kind = Shr } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus
                          ABus = value |> operandToBus
                          ALU = Some ALU.ABus
                          Shifter = Some Shifter.Left }
                    list
            | { Content = GotoInstruction { BasicBlock = branch } } ->

                if Some branch
                   <> Array.tryItem (bbIndex + 1) basicBlocks then
                    prependOperation
                        { Operation.Default with
                              Address = branch |> getName |> Some
                              Condition = Some Cond.None }
                        list
                else
                    list
            | { Content = CondBrInstruction { Kind = kind
                                              Value = Ref { Users = [ _ ]
                                                            Index = Some condIndex
                                                            Register = None }
                                              FalseBranch = falseBranch
                                              TrueBranch = trueBranch }
                Index = Some brIndex } when condIndex + 1 = brIndex ->
                // Case: conditions only use is as condition in the branch instruction and it is immediately before
                // the branch instruction. No register should have been allocated for it and we need to add the
                // conditional branch to them instruction.
                let list =
                    match list |> List.tryHead with
                    | Some (Label _)
                    | None -> failwith "Internal Compiler Error: Did not compute condition before branch"
                    | Some (Operation operation) ->
                        list
                        |> List.tail
                        |> prependOperation
                            { operation with
                                  Address = trueBranch |> getName |> Some
                                  Condition = if kind = Negative then Some Cond.Neg else Some Cond.Zero }

                if Some falseBranch = Array.tryItem (bbIndex + 1) basicBlocks then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              Address = falseBranch |> getName |> Some
                              Condition = Some Cond.None }
                        list
            | { Content = CondBrInstruction { Kind = kind
                                              Value = value
                                              FalseBranch = falseBranch
                                              TrueBranch = trueBranch } } ->
                let list =
                    prependOperation
                        { Operation.Default with
                              Address = trueBranch |> getName |> Some
                              AMux = Some AMux.ABus
                              Condition = if kind = Negative then Some Cond.Neg else Some Cond.Zero
                              ABus = value |> operandToBus
                              ALU = Some ALU.ABus
                              Shifter = None }
                        list

                if Some falseBranch = Array.tryItem (bbIndex + 1) basicBlocks then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              Address = falseBranch |> getName |> Some
                              Condition = Some Cond.None }
                        list
            | { Content = LoadInstruction { Source = value } } ->
                let list =
                    prependOperation
                        { Operation.Default with
                              MemoryAccess = Some MemoryAccess.Read
                              MARWrite = Some true
                              BBus = value |> operandToBus }
                        list

                let list =
                    prependOperation
                        { Operation.Default with
                              MemoryAccess = Some MemoryAccess.Read
                              MARWrite = Some false }
                        list

                prependOperation
                    { Operation.Default with
                          SBus = operandToBus instr
                          AMux = Some AMux.MBR
                          ALU = Some ALU.ABus
                          Shifter = Some Shifter.Noop }
                    list
            | { Content = StoreInstruction { Value = value
                                             Destination = destination } } ->
                let list =
                    prependOperation
                        { Operation.Default with
                              MemoryAccess = Some MemoryAccess.Write
                              MARWrite = Some true
                              MBRWrite = Some true
                              BBus = destination |> operandToBus
                              ABus = value |> operandToBus
                              ALU = Some ALU.ABus
                              AMux = Some AMux.ABus
                              Shifter = Some Shifter.Noop }
                        list

                prependOperation
                    { Operation.Default with
                          MemoryAccess = Some MemoryAccess.Write
                          MBRWrite = Some false
                          MARWrite = Some false }
                    list
            | { Content = PhiInstruction _ } -> list
            | _ -> failwith "Internal Compiler Error: Can't compile IR instruction to assembly") list) []
    |> List.rev

let removeRedundantLabels assemblyList =
    let usedLabels =
        assemblyList
        |> List.fold (fun set assembly ->
            match assembly with
            | Operation { Address = Some s } -> Set.add s set
            | _ -> set) (Set([]))

    assemblyList
    |> List.filter (function
        | Label s -> usedLabels |> Set.contains s
        | _ -> true)
