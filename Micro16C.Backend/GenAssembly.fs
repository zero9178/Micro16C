module Micro16C.Backend.GenAssembly

open System
open System.Collections.Generic
open Micro16C.Backend.Assembly
open Micro16C.MiddleEnd.IR



let genAssembly (irModule: Module): AssemblyLine list =

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

    let valueToRegisters =
        RegisterAllocator.allocateRegisters irModule

    let operandToBus operand =
        match !operand with
        | { Content = Constant { Value = 0s } } -> Bus.Zero
        | { Content = Constant { Value = 1s } } -> Bus.One
        | { Content = Constant { Value = -1s } } -> Bus.NegOne
        | _ -> valueToRegisters.[operand] |> Register.toBus

    let prependOperation operation list = (Operation operation) :: list

    let basicBlocks =
        irModule |> Module.basicBlocks |> Array.ofList

    basicBlocks
    |> Array.indexed
    |> Array.fold (fun list (bbIndex, bbValue) ->
        let bb = !bbValue |> Value.asBasicBlock

        let list = (bbValue |> getName |> Label) :: list

        bb
        |> BasicBlock.instructions
        |> List.fold (fun list instr ->

            match !instr with
            | { Content = BinaryInstruction { Left = Ref { Content = Constant { Value = 0x8000s } }
                                              Right = op
                                              Kind = And } }
            | { Content = BinaryInstruction { Right = Ref { Content = Constant { Value = 0x8000s } }
                                              Left = op
                                              Kind = And } } ->
                if operandToBus instr = operandToBus op then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              SBus = operandToBus instr |> Some
                              ABus = operandToBus op |> Some
                              AMux = Some AMux.ABus
                              ALU = Some ALU.ABus
                              Shifter = Some Shifter.Noop }
                        list
            | { Content = MoveInstruction { Source = op }
                Users = [ Ref { Content = PhiInstruction _ } as phi ] } ->
                if operandToBus phi = operandToBus op then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              SBus = operandToBus phi |> Some
                              ABus = operandToBus op |> Some
                              AMux = Some AMux.ABus
                              ALU = Some ALU.ABus
                              Shifter = Some Shifter.Noop }
                        list
            | { Content = MoveInstruction { Source = op } } ->
                if operandToBus instr = operandToBus op then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              SBus = operandToBus instr |> Some
                              ABus = operandToBus op |> Some
                              AMux = Some AMux.ABus
                              ALU = Some ALU.ABus
                              Shifter = Some Shifter.Noop }
                        list
            | { Content = BinaryInstruction { Left = lhs; Right = rhs; Kind = kind } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus |> Some
                          ABus = lhs |> operandToBus |> Some
                          BBus = rhs |> operandToBus |> Some
                          ALU = Some(if kind = Add then ALU.Add else ALU.And)
                          Shifter = Some Shifter.Noop }
                    list
            | { Content = UnaryInstruction { Value = value; Kind = Not } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus |> Some
                          ABus = value |> operandToBus |> Some
                          ALU = Some ALU.Neg
                          Shifter = Some Shifter.Noop }
                    list
            | { Content = UnaryInstruction { Value = value; Kind = Shl } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus |> Some
                          ABus = value |> operandToBus |> Some
                          ALU = Some ALU.ABus
                          Shifter = Some Shifter.Left }
                    list
            | { Content = UnaryInstruction { Value = value; Kind = Shr } } ->
                prependOperation
                    { Operation.Default with
                          AMux = Some AMux.ABus
                          SBus = instr |> operandToBus |> Some
                          ABus = value |> operandToBus |> Some
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
            | { Content = CondBrInstruction { Kind = Negative
                                              Value = value
                                              FalseBranch = falseBranch
                                              TrueBranch = trueBranch } } ->
                let list =
                    prependOperation
                        { Operation.Default with
                              Address = trueBranch |> getName |> Some
                              AMux = Some AMux.ABus
                              Condition = Some Cond.Neg
                              ABus = value |> operandToBus |> Some
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
            | { Content = CondBrInstruction { Kind = NotZero
                                              Value = value
                                              FalseBranch = falseBranch
                                              TrueBranch = trueBranch } } ->
                let list =
                    prependOperation
                        { Operation.Default with
                              Address = falseBranch |> getName |> Some
                              AMux = Some AMux.ABus
                              Condition = Some Cond.Zero
                              ABus = value |> operandToBus |> Some
                              ALU = Some ALU.ABus
                              Shifter = None }
                        list

                if Some trueBranch = Array.tryItem (bbIndex + 1) basicBlocks then
                    list
                else
                    prependOperation
                        { Operation.Default with
                              Address = trueBranch |> getName |> Some
                              Condition = Some Cond.None }
                        list
            | { Content = PhiInstruction _ } -> list
            | _ -> list) list) []
    |> List.rev
