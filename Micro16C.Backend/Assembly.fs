module Micro16C.Backend.Codegen

open Micro16C.MiddleEnd.IR

type AMux =
    | MBR = 0b0
    | ABus = 0b1

type Cond =
    | NoJump = 0b0
    | Neg = 0b1
    | Zero = 0b10
    | None = 0b11

type ALU =
    | ABus = 0b00
    | Add = 0b01
    | And = 0b10
    | Neg = 0b11

type Shifter =
    | Noop = 0b00
    | Left = 0b01
    | Right = 0b10

type MemoryAccess =
    | Read
    | Write

type Bus =
    | Zero = 0b0000
    | One = 0b0001
    | NegOne = 0b0010
    | PC = 0b0011
    | R0 = 0b0100
    | R1 = 0b0101
    | R2 = 0b0110
    | R3 = 0b0111
    | R4 = 0b1000
    | R5 = 0b1001
    | R6 = 0b1010
    | R7 = 0b1011
    | R8 = 0b1100
    | R9 = 0b1101
    | R10 = 0b1110
    | AC = 0b1111


module Register =

    let toBus (register: Register) =
        match register with
        | Register.PC -> Bus.PC
        | Register.AC -> Bus.AC
        | Register.R0 -> Bus.R0
        | Register.R1 -> Bus.R1
        | Register.R2 -> Bus.R2
        | Register.R3 -> Bus.R3
        | Register.R4 -> Bus.R4
        | Register.R5 -> Bus.R5
        | Register.R6 -> Bus.R6
        | Register.R7 -> Bus.R7
        | Register.R8 -> Bus.R8
        | Register.R9 -> Bus.R9
        | Register.R10 -> Bus.R10

type Operation =
    { AMux: AMux option
      Condition: Cond option
      ALU: ALU option
      Shifter: Shifter option
      MemoryAccess: MemoryAccess option
      SBus: Bus option
      BBus: Bus option
      ABus: Bus option
      Address: uint8 option }
    // Every field here is an option. None signifies that a field is not set due to not being semantically relevant
    // That makes it easier to merge multiple ops into one

    static member Default =
        { AMux = None
          Condition = None
          ALU = None
          Shifter = None
          MemoryAccess = None
          SBus = None
          BBus = None
          ABus = None
          Address = None }
