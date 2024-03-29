module Micro16C.Backend.GenAssembly

open System
open Micro16C.Backend.Assembly
open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Passes
open Micro16C.MiddleEnd.Util
open Micro16C.MiddleEnd.PassManager

let private operandToBus registers operand =
    match operand with
    | ConstOp 0s -> Bus.Zero |> Some
    | ConstOp 1s -> Bus.One |> Some
    | ConstOp -1s -> Bus.NegOne |> Some
    | RegisterOp reg -> Register.toBus reg |> Some
    | _ ->
        registers
        |> ImmutableMap.tryFind operand
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
                   Condition = None } as op2) :: rest when
        sBus = aBus
        && Operation.canCombineExclusive
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
                  AMux = None }
        ->
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
      Operation ({ Shifter = (None | Some Shifter.Noop)
                   SBus = Some sBus } as op2) :: rest when
        aBus = sBus
        && Operation.canCombineExclusive { op1 with ALU = None; ABus = None } op2
        ->
        let op =
            Operation.combine { op1 with ALU = None; ABus = None } op2

        Operation(op) :: rest
    | { ALU = Some ALU.ABus
        AMux = Some AMux.MBR
        SBus = None
        Condition = Some _ } as op1,
      Operation ({ Shifter = (None | Some Shifter.Noop)
                   MBRWrite = Some true } as op2) :: rest when
        Operation.canCombineExclusive { op1 with ALU = None; ABus = None } op2
        ->
        let op =
            Operation.combine { op1 with ALU = None; ABus = None } op2

        Operation(op) :: rest
    | op1, Operation op2 :: list when Operation.canCombineExclusive op1 op2 ->
        Operation(Operation.combine op1 op2) :: list
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

let private genPhiMoves liveInfo registers block list =

    // Following assumptions need to be true here:
    // Any block whose successor(s) contain a phi ends with an unconditional branch.
    // Therefore any such block also only has a single successor. If it does not have a single successor,
    // the successor definitely does not contain phis. This facts is guaranteed by the breakPhiCriticalEdges pass
    // run shortly before register allocation passes

    !block
    |> BasicBlock.successors
    |> Seq.tryExactlyOne
    |> Option.map ((!) >> Value.asBasicBlock >> BasicBlock.phis)
    |> Option.map
        (fun phis ->
            let transfers =
                phis
                |> Seq.fold
                    (fun list phi ->
                        match !phi
                              |> Value.operands
                              |> Seq.pairwise
                              |> Seq.find (snd >> (=) block)
                              |> fst with
                        | UndefOp -> list
                        | source ->
                            (source |> operandToBus registers |> Option.get, phi |> operandToBus registers |> Option.get)
                            :: list)
                    []

            let assigned = Array.create 13 false

            liveInfo
            |> ImmutableMap.find block
            |> LivenessInfo.liveOut
            |> Seq.map (
                fun x -> registers |> ImmutableMap.find x
                >> RegisterAllocator.registerToIndex
            )
            |> Seq.iter (fun i -> Array.set assigned i true)

            let notZeroOutDegrees seq =
                seq
                |> Seq.filter (fun (x, y) -> x <> y)
                |> Seq.countBy fst
                |> Seq.map fst

            let transfers, list =
                Seq.unfold
                    (fun (transfers, list) ->
                        let newTransfer, list =
                            transfers
                            |> List.fold
                                (fun (transfers, list) (fromReg, toReg) ->
                                    if toReg = fromReg
                                       || notZeroOutDegrees transfers |> Seq.contains toReg then
                                        (transfers, list)
                                    else
                                        let list = list |> prependMove fromReg toReg

                                        fromReg
                                        |> Bus.toRegister
                                        |> Option.iter
                                            (fun reg ->
                                                Array.set assigned (reg |> RegisterAllocator.registerToIndex) false)

                                        let transfers =
                                            transfers
                                            |> List.choose
                                                (fun (a, b) ->
                                                    if a = fromReg && b = toReg then None
                                                    else if a = fromReg then Some(toReg, b)
                                                    else Some(a, b))

                                        (transfers, list))
                                (transfers, list)

                        if List.length newTransfer = List.length transfers then
                            None
                        else
                            Some((newTransfer, list), (newTransfer, list)))
                    (transfers, list)
                |> Seq.append (Seq.singleton (transfers, list))
                |> Seq.last

            let maps =
                transfers
                |> List.filter (fun (x, y) -> x <> y)
                |> List.map (fun (x, y) -> (y, x))
                |> Map

            let temp =
                lazy
                    (match assigned |> Array.tryFindIndex not with
                     | None ->
                         failwith
                             "Too much register pressure to implement phi operation. Spilling is not yet implemented"
                     | Some i ->
                         Array.set assigned i true

                         i
                         |> RegisterAllocator.indexToRegister
                         |> Register.toBus)

            Seq.unfold
                (fun (maps, list) ->
                    if maps |> Map.isEmpty then
                        None
                    else
                        let brokenOpen = maps |> Seq.head
                        let maps = maps |> Map.remove brokenOpen.Key

                        let maps, list =
                            Seq.unfold
                                (fun (current, toReg, maps, list) ->
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

                        Some(list, (maps, list)))
                (maps, list)
            |> Seq.tryLast
            |> Option.defaultValue list)
    |> Option.defaultValue list

let private genAssembly passManager irModule : AssemblyLine list =

    let registers =
        passManager
        |> PassManager.analysisData RegisterAllocator.allocateRegistersPass

    let liveInfo =
        passManager
        |> PassManager.analysisData analyzeLivenessPass

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

    let operandToBus = operandToBus registers

    basicBlocks
    |> Array.indexed
    |> Array.fold
        (fun list (bbIndex, bbValue) ->
            let bb = !bbValue |> Value.asBasicBlock

            let list = (bbValue |> getName |> Label) :: list

            bb
            |> BasicBlock.instructions
            |> List.fold
                (fun list instr ->

                    let list =
                        if Value.isTerminating !instr then
                            genPhiMoves liveInfo registers (!instr |> Value.parentBlock |> Option.get) list
                        else
                            list

                    match instr with
                    | LoadOp (RegisterOp _ as op) ->
                        prependMove (operandToBus op |> Option.get) (operandToBus instr |> Option.get) list
                    | StoreOp (op, (RegisterOp _ as instr)) ->
                        prependMove (operandToBus op |> Option.get) (operandToBus instr |> Option.get) list
                    | BinOp Add (lhs, rhs)
                    | BinOp And (lhs, rhs) ->
                        let kind =
                            match instr with
                            | BinOp Add _ -> ALU.Add
                            | BinOp And _ -> ALU.And
                            | _ -> failwith "Internal Compiler Error"

                        prependOperation
                            { Operation.Default with
                                  AMux = Some AMux.ABus
                                  SBus = instr |> operandToBus
                                  ABus = lhs |> operandToBus
                                  BBus = rhs |> operandToBus
                                  ALU = kind |> Some
                                  Shifter = Some Shifter.Noop }
                            list
                    | UnaryOp Not value ->
                        prependOperation
                            { Operation.Default with
                                  AMux = Some AMux.ABus
                                  SBus = instr |> operandToBus
                                  ABus = value |> operandToBus
                                  ALU = Some ALU.Neg
                                  Shifter = Some Shifter.Noop }
                            list
                    | BinOp Shl (value, ConstOp 1s) ->
                        prependOperation
                            { Operation.Default with
                                  AMux = Some AMux.ABus
                                  SBus = instr |> operandToBus
                                  ABus = value |> operandToBus
                                  ALU = Some ALU.ABus
                                  Shifter = Some Shifter.Left }
                            list
                    | BinOp LShr (value, ConstOp 1s) ->
                        prependOperation
                            { Operation.Default with
                                  AMux = Some AMux.ABus
                                  SBus = instr |> operandToBus
                                  ABus = value |> operandToBus
                                  ALU = Some ALU.ABus
                                  Shifter = Some Shifter.Right }
                            list
                    | GotoOp branch ->

                        if Some branch
                           <> Array.tryItem (bbIndex + 1) basicBlocks then
                            prependOperation
                                { Operation.Default with
                                      Address = branch |> getName |> Some
                                      Condition = Some Cond.None }
                                list
                        else
                            list
                    | CondBrOp (kind, value, trueBranch, falseBranch) ->
                        let list =
                            prependOperation
                                { Operation.Default with
                                      Address = trueBranch |> getName |> Some
                                      AMux = Some AMux.ABus
                                      Condition =
                                          if kind = Negative then
                                              Some Cond.Neg
                                          else
                                              Some Cond.Zero
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
                    | LoadOp value ->
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
                    | StoreOp (value, destination) ->
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
                    | PhiOp _ -> list
                    | _ -> failwith "Internal Compiler Error: Can't compile IR instruction to assembly")
                list)
        []
    |> List.rev

let private removeUnusedLabels _ assemblyList =
    let usedLabels =
        assemblyList
        |> Seq.fold
            (fun set assembly ->
                match assembly with
                | Operation { Address = Some s } -> Set.add s set
                | _ -> set)
            (Set([]))

    assemblyList
    |> Seq.filter
        (function
        | Label s -> usedLabels |> Set.contains s
        | _ -> true)

let removeRedundantLabels _ assemblyList =
    let replacements =
        assemblyList
        |> Seq.windowed 2
        |> Seq.fold
            (fun replaced assembly ->
                match assembly with
                | [| Label first; Label second |] ->
                    match replaced |> Map.tryFind first with
                    | Some found -> replaced |> Map.add second found
                    | None -> replaced |> Map.add second first
                | _ -> replaced)
            Map.empty

    assemblyList
    |> Seq.choose
        (fun assembly ->
            match assembly with
            | Operation ({ Address = Some s } as op) ->
                match replacements |> Map.tryFind s with
                | Some s -> Operation { op with Address = Some s } |> Some
                | None -> Some assembly
            | Label s when replacements |> Map.containsKey s -> None
            | _ -> assembly |> Some)

let genMachineCode _ assemblyList =
    let machineCode, symbolTable =
        assemblyList
        |> Seq.fold
            (fun (result, symbolTable) assembly ->
                match assembly with
                | Label s ->
                    (result,
                     symbolTable
                     |> Map.add s (List.length result |> uint8))
                | Operation ({ Address = Some s } as op) ->
                    ((op |> Operation.asMachineCode, Some s) :: result, symbolTable)
                | Operation op -> ((op |> Operation.asMachineCode, None) :: result, symbolTable))
            ([], Map.empty)

    machineCode
    |> Seq.map
        (fun (instr, address) ->
            match address with
            | None -> instr
            | Some s ->
                let address = symbolTable |> Map.find s
                instr ||| (address |> int))
    |> Seq.rev

let genAssemblyPass =
    { Pass = genAssembly
      DependsOn =
          [ RegisterAllocator.allocateRegistersPass
            analyzeLivenessPass ]
      Invalidates = [] }

let removeUnusedLabelsPass =
    { Pass = removeUnusedLabels
      DependsOn = []
      Invalidates = [] }

let removeRedundantLabelsPass =
    { Pass = removeRedundantLabels
      DependsOn = []
      Invalidates = [] }

let genMachineCodePass =
    { Pass = genMachineCode
      DependsOn = []
      Invalidates = [] }
