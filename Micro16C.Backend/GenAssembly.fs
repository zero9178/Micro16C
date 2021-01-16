module Micro16C.Backend.GenAssembly

open System
open System.Collections.Generic
open Micro16C.Backend.Assembly
open Micro16C.MiddleEnd.IR



let genAssembly irModule: AssemblyLine list =

    let mutable counter = 0

    let mutable seenValues =
        Dictionary<Value, string>(HashIdentity.Reference)

    let seenNames = ref Set.empty

    let getName (value: Value ref) =
        match seenValues.TryGetValue !value with
        | (true, name) -> name
        | (false, _) ->
            match !value |> Value.name with
            | "" ->
                counter <- counter + 1
                let name = (counter - 1) |> string
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

                uniqueName (!value |> Value.name)

    let operandToBus operand =
        match !operand with
        | { Content = Constant { Value = 0s } } -> Bus.Zero |> Some
        | { Content = Constant { Value = 1s } } -> Bus.One |> Some
        | { Content = Constant { Value = -1s } } -> Bus.NegOne |> Some
        | { Content = Register reg } -> Register.toBus reg |> Some
        | _ ->
            !operand
            |> Value.register
            |> Option.map Register.toBus

    let prependOperation operation list =
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
        | op1, Operation op2 :: list when Operation.canCombine op1 op2 -> Operation(Operation.combine op1 op2) :: list
        | operation, list -> (Operation operation) :: list

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

            match !instr with
            | { Content = CopyInstruction { Source = op }
                Users = [ Ref { Content = PhiInstruction _ } as phi ] } ->
                if operandToBus phi = operandToBus op then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              SBus = operandToBus phi
                              ABus = operandToBus op
                              AMux = Some AMux.ABus
                              ALU = Some ALU.ABus
                              Shifter = Some Shifter.Noop }
                        list
            | { Content = LoadInstruction { Source = Ref { Content = Register _ } as op } }
            | { Content = CopyInstruction { Source = op } } ->
                if operandToBus instr = operandToBus op then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              SBus = operandToBus instr
                              ABus = operandToBus op
                              AMux = Some AMux.ABus
                              ALU = Some ALU.ABus
                              Shifter = Some Shifter.Noop }
                        list
            | { Content = StoreInstruction { Destination = Ref { Content = Register _ } as instr
                                             Value = op } } ->
                if operandToBus instr = operandToBus op then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              SBus = operandToBus instr
                              ABus = operandToBus op
                              AMux = Some AMux.ABus
                              ALU = Some ALU.ABus
                              Shifter = Some Shifter.Noop }
                        list
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
