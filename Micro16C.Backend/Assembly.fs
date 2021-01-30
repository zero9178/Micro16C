module Micro16C.Backend.Assembly

open Micro16C.MiddleEnd.IR

type AMux =
    | ABus = 0b0
    | MBR = 0b1

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
    | Read = 1
    | Write = 0

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

module Bus =

    let toString bus =
        match bus with
        | Bus.Zero -> "0"
        | Bus.One -> "1"
        | Bus.NegOne -> "(-1)"
        | Bus.PC -> "PC"
        | Bus.R0 -> "R0"
        | Bus.R1 -> "R1"
        | Bus.R2 -> "R2"
        | Bus.R3 -> "R3"
        | Bus.R4 -> "R4"
        | Bus.R5 -> "R5"
        | Bus.R6 -> "R6"
        | Bus.R7 -> "R7"
        | Bus.R8 -> "R8"
        | Bus.R9 -> "R9"
        | Bus.R10 -> "R10"
        | Bus.AC -> "AC"
        | _ -> failwith "Internal Compiler Error: Invalid bus value"

    let toRegister bus =
        match bus with
        | Bus.Zero
        | Bus.One
        | Bus.NegOne -> None
        | Bus.PC -> PC |> Some
        | Bus.R0 -> R0 |> Some
        | Bus.R1 -> R1 |> Some
        | Bus.R2 -> R2 |> Some
        | Bus.R3 -> R3 |> Some
        | Bus.R4 -> R4 |> Some
        | Bus.R5 -> R5 |> Some
        | Bus.R6 -> R6 |> Some
        | Bus.R7 -> R7 |> Some
        | Bus.R8 -> R8 |> Some
        | Bus.R9 -> R9 |> Some
        | Bus.R10 -> R10 |> Some
        | Bus.AC -> AC |> Some
        | _ -> failwith "Internal Compiler Error: Invalid bus value"

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
      MARWrite: bool option
      MBRWrite: bool option
      SBus: Bus option
      BBus: Bus option
      ABus: Bus option
      Address: string option }
    // Every field here is an option. None signifies that a field is not set due to not being semantically relevant
    // That makes it easier to merge multiple ops into one

    static member Default =
        { AMux = None
          Condition = None
          ALU = None
          Shifter = None
          MemoryAccess = None
          MARWrite = None
          MBRWrite = None
          SBus = None
          BBus = None
          ABus = None
          Address = None }

    override this.ToString() =
        let op =
            match (this.ALU, this.AMux, this.ABus, this.BBus) with
            | (None, _, _, _) -> ""
            | (Some ALU.ABus, Some AMux.MBR, _, _) -> "MBR"
            | (Some ALU.ABus, Some AMux.ABus, Some a, _) -> Bus.toString a
            | (Some ALU.Add, Some AMux.MBR, _, Some b) -> "MBR + " + Bus.toString b
            | (Some ALU.Add, Some AMux.ABus, Some a, Some b) -> Bus.toString a + " + " + Bus.toString b
            | (Some ALU.And, Some AMux.MBR, _, Some b) -> "MBR & " + Bus.toString b
            | (Some ALU.And, Some AMux.ABus, Some a, Some b) -> Bus.toString a + " & " + Bus.toString b
            | (Some ALU.Neg, Some AMux.MBR, _, _) -> "~MBR"
            | (Some ALU.Neg, Some AMux.ABus, Some a, _) -> "~" + Bus.toString a
            | _ -> failwith "Internal Compiler Error: Illegally formed assembly instruction"

        let op =
            match this.Shifter with
            | None
            | Some Shifter.Noop -> op
            | Some Shifter.Left -> sprintf "lsh(%s)" op
            | Some Shifter.Right -> sprintf "rsh(%s)" op
            | _ -> failwith "Internal Compiler Error: Illegally formed assembly instruction"

        let op =
            match this.SBus with
            | Some s -> Bus.toString s + " <- " + op
            | None -> op

        let op =
            match (op, this.MBRWrite, this.SBus) with
            | (op, Some false, _)
            | (op, None, _) -> op
            | (op, Some true, None) -> sprintf "MBR <- %s" op
            | (op, Some true, Some _) -> sprintf "%s; MBR <- %s" op op

        let op =
            match (op, this.SBus, this.Condition, this.Address) with
            | (op, _, None, _)
            | (op, _, Some Cond.NoJump, _) -> op
            | ("", None, Some Cond.Neg, Some s) -> sprintf "if N goto .%s" s
            | (op, None, Some Cond.Neg, Some s) -> sprintf "(%s); if N goto .%s" op s
            | ("", None, Some Cond.Zero, Some s) -> sprintf "if Z goto .%s" s
            | (op, None, Some Cond.Zero, Some s) -> sprintf "(%s); if Z goto .%s" op s
            | ("", None, Some Cond.None, Some s) -> sprintf "goto .%s" s
            | (op, None, Some Cond.None, Some s) -> sprintf "(%s); goto .%s" op s
            | ("", Some _, Some Cond.Neg, Some s) -> sprintf "if N goto .%s" s
            | (op, Some _, Some Cond.Neg, Some s) -> sprintf "%s; if N goto .%s" op s
            | ("", Some _, Some Cond.Zero, Some s) -> sprintf "if Z goto .%s" s
            | (op, Some _, Some Cond.Zero, Some s) -> sprintf "%s; if Z goto .%s" op s
            | ("", Some _, Some Cond.None, Some s) -> sprintf "goto .%s" s
            | (op, Some _, Some Cond.None, Some s) -> sprintf "%s; goto .%s" op s
            | _ -> failwith "Internal Compiler Error: Illegally formed assembly instruction"

        let op =
            match (op, this.MARWrite, this.BBus) with
            | (op, Some false, _)
            | (op, None, _) -> op
            | ("", Some true, Some b) -> sprintf "MAR <- %s" (Bus.toString b)
            | (op, Some true, Some b) -> sprintf "%s; MAR <- %s" op (Bus.toString b)
            | _ -> failwith "Internal Compiler Error: Illegally formed assembly instruction"

        let op =
            match (op, this.MemoryAccess) with
            | (op, None) -> op
            | ("", Some MemoryAccess.Read) -> "rd"
            | ("", Some MemoryAccess.Write) -> "wr"
            | (op, Some MemoryAccess.Read) -> sprintf "%s; rd" op
            | (op, Some MemoryAccess.Write) -> sprintf "%s; wr" op
            | _ -> failwith "Internal Compiler Error: Illegally formed assembly instruction"

        op

module Operation =

    let canCombine op1 op2 =

        let comp optional1 optional2 =
            match optional1, optional2 with
            | None, None
            | None, Some _ -> true
            | Some _, None -> true
            | Some value1, Some value2 -> value1 = value2

        comp op1.AMux op2.AMux
        && comp op1.Condition op2.Condition
        && comp op1.ALU op2.ALU
        && comp op1.Shifter op2.Shifter
        && comp op1.MemoryAccess op2.MemoryAccess
        && comp op1.MARWrite op2.MARWrite
        && comp op1.MBRWrite op2.MBRWrite
        && comp op1.SBus op2.SBus
        && comp op1.BBus op2.BBus
        && comp op1.ABus op2.ABus
        && comp op1.Address op2.Address

    let combine op1 op2 =
        let optionalXOR optional1 optional2 =
            match (optional1, optional2) with
            | None, None -> None
            | Some value, None -> Some value
            | None, Some value -> Some value
            | Some value1, Some value2 ->
                assert (value1 = value2)
                Some value1

        { AMux = optionalXOR op1.AMux op2.AMux
          Condition = optionalXOR op1.Condition op2.Condition
          ALU = optionalXOR op1.ALU op2.ALU
          Shifter = optionalXOR op1.Shifter op2.Shifter
          MemoryAccess = optionalXOR op1.MemoryAccess op2.MemoryAccess
          MARWrite = optionalXOR op1.MARWrite op2.MARWrite
          MBRWrite = optionalXOR op1.MBRWrite op2.MBRWrite
          SBus = optionalXOR op1.SBus op2.SBus
          BBus = optionalXOR op1.BBus op2.BBus
          ABus = optionalXOR op1.ABus op2.ABus
          Address = optionalXOR op1.Address op2.Address }

    let canCombineExclusive op1 op2 =

        let comp optional1 optional2 =
            match optional1, optional2 with
            | None, None
            | None, Some _ -> true
            | Some _, None -> true
            | Some _, Some _ -> false

        comp op1.AMux op2.AMux
        && comp op1.Condition op2.Condition
        && comp op1.ALU op2.ALU
        && comp op1.Shifter op2.Shifter
        && (((op1.MemoryAccess |> Option.isSome)
             && (op2.MemoryAccess |> Option.isSome))
            |> not)
        && (((op1.MemoryAccess |> Option.isSome)
             && (op2.MARWrite |> Option.isSome))
            |> not)
        && (((op2.MemoryAccess |> Option.isSome)
             && (op1.MARWrite |> Option.isSome))
            |> not)
        && (((op1.MemoryAccess |> Option.isSome)
             && (op2.MBRWrite |> Option.isSome))
            |> not)
        && (((op2.MemoryAccess |> Option.isSome)
             && (op1.MBRWrite |> Option.isSome))
            |> not)
        && (((op1.MemoryAccess |> Option.isSome)
             && (op2.AMux = Some AMux.MBR))
            |> not)
        && (((op2.MemoryAccess |> Option.isSome)
             && (op1.AMux = Some AMux.MBR))
            |> not)
        && comp op1.MARWrite op2.MARWrite
        && comp op1.MBRWrite op2.MBRWrite
        && comp op1.SBus op2.SBus
        && comp op1.BBus op2.BBus
        && comp op1.ABus op2.ABus
        && comp op1.Address op2.Address

    let asText (op: Operation) = op.ToString()

    let asMachineCode op =
        let current = 0

        let current =
            current
            ||| (op.AMux |> Option.defaultValue AMux.ABus |> int
                 <<< 31)


        let current =
            current
            ||| (op.Condition
                 |> Option.defaultValue Cond.NoJump
                 |> int
                 <<< 29)


        let current =
            current
            ||| (op.ALU |> Option.defaultValue ALU.ABus |> int
                 <<< 27)


        let current =
            current
            ||| (op.Shifter
                 |> Option.defaultValue Shifter.Noop
                 |> int
                 <<< 25)


        let current =
            current
            ||| ((if op.MBRWrite |> Option.defaultValue false then 1 else 0)
                 <<< 24)


        let current =
            current
            ||| ((if op.MARWrite |> Option.defaultValue false then 1 else 0)
                 <<< 23)


        let current =
            current
            ||| (op.MemoryAccess
                 |> Option.defaultValue MemoryAccess.Write
                 |> int
                 <<< 22)


        let current =
            current
            ||| ((if op.MemoryAccess |> Option.isSome then 1 else 0)
                 <<< 21)


        let current =
            current
            ||| ((if op.SBus |> Option.isSome then 1 else 0) <<< 20)


        let current =
            current
            ||| (op.SBus |> Option.defaultValue Bus.Zero |> int
                 <<< 16)


        let current =
            current
            ||| (op.BBus |> Option.defaultValue Bus.Zero |> int
                 <<< 12)


        let current =
            current
            ||| (op.ABus |> Option.defaultValue Bus.Zero |> int
                 <<< 8)


        current

    let fromMachineCode (m: int) =

        let get mask pos value =
            (uint (value &&& (mask <<< pos)) >>> pos) |> int

        { AMux =
              m
              |> get 0x1 31
              |> LanguagePrimitives.EnumOfValue
              |> Some
          Condition =
              m
              |> get 0x3 29
              |> LanguagePrimitives.EnumOfValue
              |> Some
          ALU =
              m
              |> get 0x3 27
              |> LanguagePrimitives.EnumOfValue
              |> Some
          Shifter =
              m
              |> get 0x3 25
              |> LanguagePrimitives.EnumOfValue
              |> Some
          MBRWrite = m |> get 0x1 24 <> 0 |> Some
          MARWrite = m |> get 0x1 23 <> 0 |> Some
          MemoryAccess =
              m
              |> get 0x1 22
              |> LanguagePrimitives.EnumOfValue
              |> Some
              |> Option.filter (fun _ -> get 0x1 21 m <> 0)
          SBus =
              m
              |> get 0xF 16
              |> LanguagePrimitives.EnumOfValue
              |> Some
              |> Option.filter (fun _ -> get 0x1 20 m <> 0)
          BBus =
              m
              |> get 0xF 12
              |> LanguagePrimitives.EnumOfValue
              |> Some
          ABus =
              m
              |> get 0xF 8
              |> LanguagePrimitives.EnumOfValue
              |> Some
          Address = m |> get 0xFF 0 |> string |> Some }

type AssemblyLine =
    | Operation of Operation
    | Label of string

let asText assemblyLine =
    assemblyLine
    |> Seq.fold (fun c x ->
        match x with
        | Label s -> c + sprintf ":%s\n" s
        | Operation s -> c + sprintf "%s\n" (s.ToString())) ""

let printAssembly assemblyLine =
    assemblyLine |> asText |> printfn "%s"

    assemblyLine
