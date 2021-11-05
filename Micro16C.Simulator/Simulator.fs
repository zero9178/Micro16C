module Micro16C.Simulator.Simulator

open Micro16C.Backend
open Micro16C.Backend.Assembly

type State =
    { MBR: int16
      MAR: int16
      AC: int16
      PC: int16
      Registers: int16 []
      Memory: int16 [] }

    static member Default =
        { MBR = 0s
          MAR = 0s
          AC = 0s
          PC = 0s
          Registers = Array.zeroCreate 11
          Memory = Array.zeroCreate 65536 }

type private MemoryAccess =
    | Read of uint16
    | Write of Address: uint16 * Value: int16

type private Context =
    { State: State
      ProgramCounter: uint8
      MemoryAccess: MemoryAccess option
      Program: Operation [] }

let simulateWithState initialState machineCode =
    let program =
        machineCode
        |> Seq.map Operation.fromMachineCode
        |> Array.ofSeq

    let readBus state bus =
        match bus with
        | Bus.Zero -> 0s
        | Bus.One -> 1s
        | Bus.NegOne -> -1s
        | Bus.AC -> state.AC
        | Bus.PC -> state.PC
        | Bus.R0 -> state.Registers.[0]
        | Bus.R1 -> state.Registers.[1]
        | Bus.R2 -> state.Registers.[2]
        | Bus.R3 -> state.Registers.[3]
        | Bus.R4 -> state.Registers.[4]
        | Bus.R5 -> state.Registers.[5]
        | Bus.R6 -> state.Registers.[6]
        | Bus.R7 -> state.Registers.[7]
        | Bus.R8 -> state.Registers.[8]
        | Bus.R9 -> state.Registers.[9]
        | Bus.R10 -> state.Registers.[10]
        | _ -> failwith "Invalid Bus value"

    let setArray index value array =
        array
        |> Array.mapi (fun i v -> if i = index then value else v)


    Seq.unfold
        (fun context ->
            if (context.ProgramCounter |> int)
               >= Array.length context.Program then
                None
            else
                let instr =
                    context.Program.[context.ProgramCounter |> int]

                let aBus =
                    match instr with
                    | { AMux = (Some AMux.ABus | None)
                        ABus = Some aBus } -> readBus context.State aBus
                    | { AMux = Some AMux.MBR } -> context.State.MBR
                    | _ -> 0s

                let bBus =
                    instr.BBus
                    |> Option.defaultValue Bus.Zero
                    |> readBus context.State

                let aluOutput =
                    match instr with
                    | { ALU = Some ALU.ABus } -> aBus
                    | { ALU = Some ALU.Add } -> aBus + bBus
                    | { ALU = Some ALU.And } -> aBus &&& bBus
                    | { ALU = Some ALU.Neg } -> ~~~aBus
                    | _ -> 0s

                let zeroFlag = aluOutput = 0s
                let negFlag = aluOutput < 0s

                let sBus =
                    match instr with
                    | { Shifter = Some Shifter.Left } -> aluOutput <<< 1
                    | { Shifter = Some Shifter.Right } -> ((aluOutput |> uint16) >>> 1) |> int16
                    | _ -> aluOutput

                let context =
                    match instr with
                    | { SBus = Some Bus.PC } ->
                        { context with
                              State = { context.State with PC = sBus } }
                    | { SBus = Some Bus.AC } ->
                        { context with
                              State = { context.State with AC = sBus } }
                    | { SBus = Some Bus.R0 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 0 sBus context.State.Registers } }
                    | { SBus = Some Bus.R1 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 1 sBus context.State.Registers } }
                    | { SBus = Some Bus.R2 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 2 sBus context.State.Registers } }
                    | { SBus = Some Bus.R3 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 3 sBus context.State.Registers } }
                    | { SBus = Some Bus.R4 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 4 sBus context.State.Registers } }
                    | { SBus = Some Bus.R5 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 5 sBus context.State.Registers } }
                    | { SBus = Some Bus.R6 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 6 sBus context.State.Registers } }
                    | { SBus = Some Bus.R7 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 7 sBus context.State.Registers } }
                    | { SBus = Some Bus.R8 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 8 sBus context.State.Registers } }
                    | { SBus = Some Bus.R9 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 9 sBus context.State.Registers } }
                    | { SBus = Some Bus.R10 } ->
                        { context with
                              State =
                                  { context.State with
                                        Registers = setArray 10 sBus context.State.Registers } }
                    | _ -> context

                let context =
                    match instr with
                    | { MBRWrite = Some true } ->
                        { context with
                              State = { context.State with MBR = sBus } }
                    | _ -> context

                let context =
                    match instr with
                    | { MARWrite = Some true } ->
                        { context with
                              State = { context.State with MAR = bBus } }
                    | _ -> context

                let context =
                    match (context, instr) with
                    | { MemoryAccess = None }, { MemoryAccess = Some Assembly.MemoryAccess.Read } ->
                        { context with
                              MemoryAccess = Read(context.State.MAR |> uint16) |> Some }
                    | { MemoryAccess = None }, { MemoryAccess = Some Assembly.MemoryAccess.Write } ->
                        { context with
                              MemoryAccess =
                                  Write(context.State.MAR |> uint16, context.State.MBR)
                                  |> Some }
                    | { MemoryAccess = Some (Read address) }, { MemoryAccess = Some Assembly.MemoryAccess.Read } ->
                        { context with
                              MemoryAccess = None
                              State =
                                  { context.State with
                                        MBR = context.State.Memory.[address |> int] } }
                    | { MemoryAccess = Some (Write (address, value)) },
                      { MemoryAccess = Some Assembly.MemoryAccess.Write } ->
                        { context with
                              MemoryAccess = None
                              State =
                                  { context.State with
                                        Memory =
                                            context.State.Memory
                                            |> setArray (address |> int) value } }
                    | { MemoryAccess = Some _ }, _ -> { context with MemoryAccess = None }
                    | _ -> context

                match instr with
                | { Condition = Some Cond.None
                    Address = Some address } ->
                    Some(
                        context.State,
                        { context with
                              ProgramCounter = address |> uint8 }
                    )
                | { Condition = Some Cond.Neg
                    Address = Some address } when negFlag ->
                    Some(
                        context.State,
                        { context with
                              ProgramCounter = address |> uint8 }
                    )
                | { Condition = Some Cond.Zero
                    Address = Some address } when zeroFlag ->
                    Some(
                        context.State,
                        { context with
                              ProgramCounter = address |> uint8 }
                    )
                | _ ->
                    Some(
                        context.State,
                        { context with
                              ProgramCounter = context.ProgramCounter + 1uy }
                    )


            )
        { State = initialState
          ProgramCounter = 0uy
          MemoryAccess = None
          Program = program }
    |> Seq.append (Seq.singleton initialState)

let simulate machineCode =
    simulateWithState State.Default machineCode
