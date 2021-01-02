module Micro16C.Backend.GenAssembly

open System.Collections.Immutable
open Micro16C.Backend.Assembly
open Micro16C.MiddleEnd.IR

let genAssembly (irModule: Module): AssemblyLine list =
    let valueToRegisters =
        RegisterAllocator.allocateRegisters irModule

    let operandToBus operand =
        match !operand with
        | { Content = Constant { Value = 0s } } -> Bus.Zero
        | { Content = Constant { Value = 1s } } -> Bus.One
        | { Content = Constant { Value = -1s } } -> Bus.NegOne
        | _ -> valueToRegisters.[operand] |> Register.toBus

    let prependOperation operation list = (Operation operation) :: list

    let mutable visitedBlocks =
        ImmutableHashSet.Create<Value ref>(HashIdentity.Reference)

    let genPhi instr list =
        match (!instr).ParentBlock with
        | None -> failwith "Internal Compiler Error: Instruction with no parent block"
        | Some parentBlock ->
            !parentBlock
            |> BasicBlock.successors
            |> List.fold (fun list succ ->
                !succ
                |> Value.asBasicBlock
                |> BasicBlock.phis
                |> List.fold (fun list phi ->
                    match !phi with
                    | { Content = PhiInstruction { Incoming = incoming } } ->
                        let (op, _) =
                            incoming |> List.find (snd >> ((=) parentBlock))

                        if operandToBus op = operandToBus phi then
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
                    | _ -> failwith "Internal Compiler Error: Expected Phi Instruction") list) list

    let rec genAssemblyForBlock list bbValue =
        if visitedBlocks.Contains bbValue then
            list
        else
            visitedBlocks <- visitedBlocks.Add(bbValue)
            let bb = !bbValue |> Value.asBasicBlock

            let list =
                (!bbValue |> Value.name |> Label) :: list


            bb
            |> BasicBlock.instructions
            |> List.fold (fun list instr ->

                let list =
                    if Value.isTerminating !instr then genPhi instr list else list

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
                    if visitedBlocks.Contains branch then
                        prependOperation
                            { Operation.Default with
                                  Address = !branch |> Value.name |> Some
                                  Condition = Some Cond.None }
                            list
                    else
                        genAssemblyForBlock list branch
                | { Content = CondBrInstruction { Condition = Ref { Content = BinaryInstruction { Kind = And
                                                                                                  Right = Ref { Content = Constant { Value = 0x8000s } } } } as value
                                                  FalseBranch = falseBranch
                                                  TrueBranch = trueBranch } }
                | { Content = CondBrInstruction { Condition = Ref { Content = BinaryInstruction { Kind = And
                                                                                                  Left = Ref { Content = Constant { Value = 0x8000s } } } } as value
                                                  FalseBranch = falseBranch
                                                  TrueBranch = trueBranch } } ->
                    let list =
                        prependOperation
                            { Operation.Default with
                                  Address = !trueBranch |> Value.name |> Some
                                  AMux = Some AMux.ABus
                                  Condition = Some Cond.Neg
                                  ABus = value |> operandToBus |> Some
                                  ALU = Some ALU.ABus
                                  Shifter = None }
                            list

                    let list = genAssemblyForBlock list falseBranch
                    genAssemblyForBlock list trueBranch
                | { Content = CondBrInstruction { Condition = value
                                                  FalseBranch = falseBranch
                                                  TrueBranch = trueBranch } } ->
                    let list =
                        prependOperation
                            { Operation.Default with
                                  Address = !falseBranch |> Value.name |> Some
                                  AMux = Some AMux.ABus
                                  Condition = Some Cond.Zero
                                  ABus = value |> operandToBus |> Some
                                  ALU = Some ALU.ABus
                                  Shifter = None }
                            list

                    let list = genAssemblyForBlock list trueBranch
                    genAssemblyForBlock list falseBranch
                | { Content = PhiInstruction _ } -> list
                | _ -> list) list

    match irModule
          |> Module.basicBlocks
          |> List.tryHead
          |> Option.map (genAssemblyForBlock []) with
    | None -> []
    | Some l -> l |> List.rev
