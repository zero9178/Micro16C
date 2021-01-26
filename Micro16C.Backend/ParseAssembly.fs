module Micro16C.Backend.ParseAssembly

open System
open Micro16C.Backend.Assembly

(*
IDENTIFIER = ([^\W\d]|[.$])[\w.$]*
ONE = "1"
ZERO = "0"
MINUS = "-"
ARROW = "<-"
NOT = "~"
OPEN_PARENTHESES = "("
CLOSE_PARENTHESES = ")"
SEMI_COLON = ";"
COLON = ":"
DOT = "."
AND = "&"
PLUS = "+"
COMMENT = "#".*
*)

type private TokenType =
    | Identifier of string
    | One
    | Zero
    | Minus
    | Arrow
    | Not
    | OpenParentheses
    | CloseParentheses
    | SemiColon
    | Colon
    | Dot
    | And
    | Plus

let private tokenize chars =
    Seq.unfold (fun chars ->

        let chars =
            chars |> List.skipWhile Char.IsWhiteSpace

        let chars =
            match chars with
            | '#' :: chars ->
                chars
                |> List.skipWhile ((<>) '\n')
                |> List.skipWhile Char.IsWhiteSpace
            | _ -> chars

        match chars with
        | [] -> None
        | '<' :: '-' :: chars -> Some(Arrow, chars)
        | '1' :: chars -> Some(One, chars)
        | '0' :: chars -> Some(Zero, chars)
        | '-' :: chars -> Some(Minus, chars)
        | '~' :: chars -> Some(Not, chars)
        | '(' :: chars -> Some(OpenParentheses, chars)
        | ')' :: chars -> Some(CloseParentheses, chars)
        | ';' :: chars -> Some(SemiColon, chars)
        | ':' :: chars -> Some(Colon, chars)
        | '.' :: chars -> Some(Dot, chars)
        | '&' :: chars -> Some(And, chars)
        | '+' :: chars -> Some(Plus, chars)
        | (c :: _) as chars when Char.IsLetter c || c = '_' || c = '$' ->
            let identifier =
                chars
                |> List.takeWhile (fun c ->
                    Char.IsLetterOrDigit c
                    || c = '_'
                    || c = '$'
                    || c = '.')

            let chars =
                chars |> List.skip (List.length identifier)

            Some(identifier |> Array.ofList |> String |> Identifier, chars)
        | c :: _ -> failwithf "Invalid character '%c'" c) chars
    |> Seq.cache

(*
Grammar:

<Register> ::= "R0" | "R1" | "R2" | "R3" | "R4" | "R5" | "R6" | "R7" | "R8" | "R9" | "R10" | "AC" | "PC" | "MBR" | "MAR"

<Operand> ::= <Register> | ONE | ZERO | MINUS ONE | OPEN_PARENTHESES <Operation> CLOSE_PARENTHESES

<Operation> ::= lsh OPEN_PARENTHESES <Operation> CLOSE_PARENTHESES
              | rhs OPEN_PARENTHESES <Operation> CLOSE_PARENTHESES
              | ~<Operand>
              | <Operand> + <Operand>
              | <Operand> & <Operand>
              | <Operand>

<Instruction> ::= ([<Register> ARROW ] <Operation> | ["if" ("N" | "Z")] "goto" DOT IDENTIFIER | "wr" | "rd")

<Label> ::= COLON IDENTIFIER

<File> ::= { <Instruction> { SEMI_COLON <Instruction> } | <Label> }

*)

type private AssemblyRegister =
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
    | PC
    | AC
    | MBR
    | MAR

type private Operand =
    | RegisterOperand of AssemblyRegister
    | One
    | Zero
    | NegOne
    | Parentheses of Operation

and private Operation =
    | LeftShift of Operation
    | RightShift of Operation
    | Negate of Operand
    | Add of Operand * Operand
    | And of Operand * Operand
    | Read of Operand

type private Condition =
    | CondZero
    | CondNegative

type private Instruction =
    | Operation of AssemblyRegister option * Operation
    | Control of Condition option * string
    | MemoryAccess of MemoryAccess

type private Line =
    | InstructionLine of Instruction list
    | LabelLine of string

type private File = { Lines: Line list }

let private lookAhead n (seq: seq<_>) =
    use e = seq.GetEnumerator()

    [ 1 .. n ]
    |> List.fold (fun result _ -> if e.MoveNext() then e.Current :: result else result) []
    |> List.rev

let private require tokenType tokens =
    match Seq.tryHead tokens with
    | None -> failwithf "Expected '%A'" tokenType
    | Some t when t = tokenType -> (t, Seq.tail tokens)
    | Some t -> failwithf "Expected '%A' instead of '%A'" tokenType t

let private parseRegister tokens =
    match Seq.tryHead tokens with
    | None -> failwithf "Expected Register"
    | Some (Identifier "R0") -> (R0, Seq.tail tokens)
    | Some (Identifier "R1") -> (R1, Seq.tail tokens)
    | Some (Identifier "R2") -> (R2, Seq.tail tokens)
    | Some (Identifier "R3") -> (R3, Seq.tail tokens)
    | Some (Identifier "R4") -> (R4, Seq.tail tokens)
    | Some (Identifier "R5") -> (R5, Seq.tail tokens)
    | Some (Identifier "R6") -> (R6, Seq.tail tokens)
    | Some (Identifier "R7") -> (R7, Seq.tail tokens)
    | Some (Identifier "R8") -> (R8, Seq.tail tokens)
    | Some (Identifier "R9") -> (R9, Seq.tail tokens)
    | Some (Identifier "R10") -> (R10, Seq.tail tokens)
    | Some (Identifier "AC") -> (AC, Seq.tail tokens)
    | Some (Identifier "PC") -> (PC, Seq.tail tokens)
    | Some (Identifier "MAR") -> (MAR, Seq.tail tokens)
    | Some (Identifier "MBR") -> (MBR, Seq.tail tokens)
    | Some t -> failwithf "Expected Register instead of '%A'" t

let rec private parseOperand tokens =
    match Seq.tryHead tokens with
    | Some Minus -> (NegOne, tokens |> Seq.tail |> require TokenType.One |> snd)
    | Some TokenType.One -> (One, tokens |> Seq.tail)
    | Some TokenType.Zero -> (Zero, tokens |> Seq.tail)
    | Some OpenParentheses ->
        let operand, tokens = tokens |> Seq.tail |> parseOperation
        let _, tokens = tokens |> require CloseParentheses
        (Parentheses operand, tokens)
    | _ ->
        let register, tokens = tokens |> parseRegister
        (RegisterOperand register, tokens)

and private parseOperation tokens =
    match Seq.tryHead tokens with
    | Some (Identifier ("lsh"
    | "rsh" as s)) ->
        let tokens =
            tokens
            |> Seq.tail
            |> require OpenParentheses
            |> snd

        let operation, tokens = parseOperation tokens

        let tokens =
            tokens |> require CloseParentheses |> snd

        if s = "lsh" then (LeftShift operation, tokens) else (RightShift operation, tokens)
    | Some TokenType.Not ->
        let tokens = tokens |> Seq.tail
        let operand, tokens = parseOperand tokens
        (Negate operand, tokens)
    | _ ->
        let operand, tokens = parseOperand tokens

        match tokens |> Seq.tryHead with
        | Some (Plus
        | TokenType.And as op) ->
            let other, tokens = tokens |> Seq.tail |> parseOperand
            if op = Plus then (Add(operand, other), tokens) else (And(operand, other), tokens)
        | _ -> (Read operand, tokens)

let rec private parseInstruction tokens =

    match tokens |> lookAhead 1 with
    | [ Identifier "goto" ]
    | [ Identifier "if" ] ->

        let condition, tokens =
            match Seq.tryHead tokens with
            | Some (Identifier "if") ->
                let tokens = tokens |> Seq.tail

                match Seq.tryHead tokens with
                | Some (Identifier "N") -> (CondNegative |> Some, tokens |> Seq.tail)
                | Some (Identifier "Z") -> (CondZero |> Some, tokens |> Seq.tail)
                | _ -> failwith "Expected 'N' or 'Z' condition after 'if'"
            | _ -> (None, tokens)

        let tokens =
            tokens
            |> require (Identifier "goto")
            |> snd
            |> require Dot
            |> snd

        match tokens |> Seq.tryHead with
        | Some (Identifier s) -> ((condition, s) |> Control, tokens |> Seq.tail)
        | _ -> failwith "Expected identifier after 'goto'"
    | [ Identifier "rd" ] -> (MemoryAccess.Read |> MemoryAccess, tokens |> Seq.tail)
    | [ Identifier "wr" ] -> (MemoryAccess.Write |> MemoryAccess, tokens |> Seq.tail)
    | _ ->
        let destination, tokens =
            match lookAhead 2 tokens with
            | [ _; Arrow ] ->
                let register, tokens = parseRegister tokens
                let _, tokens = require Arrow tokens
                (Some register, tokens)
            | _ -> (None, tokens)

        let operation, tokens = parseOperation tokens
        (Operation(destination, operation), tokens)


let private parseFile tokens =
    { Lines =
          Seq.unfold (fun tokens ->
              match Seq.tryHead tokens with
              | None -> None
              | Some Colon ->
                  let tokens = tokens |> Seq.tail

                  match tokens |> Seq.tryHead with
                  | Some (Identifier s) -> Some(LabelLine s, tokens |> Seq.tail)
                  | _ -> failwith "Expected identifier after ':'"
              | _ ->
                  let result, tokens = parseInstruction tokens

                  let rec parseInstructionList tokens =
                      match tokens |> Seq.tryHead with
                      | Some SemiColon ->
                          let result, tokens = tokens |> Seq.tail |> parseInstruction
                          let list, tokens = parseInstructionList tokens
                          (result :: list, tokens)
                      | _ -> ([], tokens)

                  let list, tokens = parseInstructionList tokens
                  Some(InstructionLine(result :: list), tokens)) tokens
          |> List.ofSeq }

let rec private visitOperand operand =
    match operand with
    | One ->
        { Operation.Default with
              AMux = Some AMux.ABus
              ABus = Some Bus.One }
    | Zero ->
        { Operation.Default with
              AMux = Some AMux.ABus
              ABus = Some Bus.One }
    | NegOne ->
        { Operation.Default with
              AMux = Some AMux.ABus
              ABus = Some Bus.NegOne }
    | Parentheses operand -> visitOperation operand
    | RegisterOperand reg ->
        match reg with
        | R0 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R0 }
        | R1 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R1 }
        | R2 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R2 }
        | R3 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R3 }
        | R4 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R4 }
        | R5 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R5 }
        | R6 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R6 }
        | R7 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R7 }
        | R8 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R8 }
        | R9 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R9 }
        | R10 ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.R10 }
        | AC ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.AC }
        | PC ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some Bus.PC }
        | MBR ->
            { Operation.Default with
                  AMux = Some AMux.MBR }
        | MAR -> failwith "MAR Register is write only"

and private visitOperation operation =
    match operation with
    | Read operand -> visitOperand operand
    | Negate operand ->
        { visitOperand operand with
              ALU = Some ALU.Neg }
    | LeftShift operation ->
        let op = visitOperation operation

        match op with
        | { Shifter = (None
            | Some Shifter.Noop) } -> { op with Shifter = Some Shifter.Left }
        | _ -> failwith "Operation can't use the shifter twice"
    | RightShift operation ->
        let op = visitOperation operation

        match op with
        | { Shifter = (None
            | Some Shifter.Noop) } -> { op with Shifter = Some Shifter.Right }
        | _ -> failwith "Operation can't use the shifter twice"
    | Add (lhs, rhs)
    | And (lhs, rhs) ->
        let lhs = visitOperand lhs
        let rhs = visitOperand rhs

        let kind =
            match operation with
            | Add _ -> ALU.Add
            | And _ -> ALU.And
            | _ -> failwith "Internal Compiler Error"

        match (lhs, rhs) with
        | { AMux = Some AMux.MBR }, { AMux = Some AMux.MBR } -> failwith "Cannot read MBR twice"
        | { ABus = Some other }, { AMux = Some AMux.MBR }
        | { AMux = Some AMux.MBR }, { ABus = Some other } ->
            { Operation.Default with
                  AMux = Some AMux.MBR
                  BBus = Some other
                  ALU = Some kind }
        | { ABus = Some lhs }, { ABus = Some rhs } ->
            { Operation.Default with
                  AMux = Some AMux.ABus
                  ABus = Some lhs
                  BBus = Some rhs
                  ALU = Some kind }
        | _ -> failwith "Internal Compiler Error"

let private visitInstructions instr =
    let op = Operation.Default

    let visitOperation current maybeDest operation =
        let op =
            match maybeDest with
            | None -> Operation.Default
            | Some MBR ->
                { Operation.Default with
                      MBRWrite = Some true }
            | Some MAR ->
                { Operation.Default with
                      MARWrite = Some true }
            | Some reg ->
                { Operation.Default with
                      SBus =
                          (match reg with
                           | R0 -> Bus.R0
                           | R1 -> Bus.R1
                           | R2 -> Bus.R2
                           | R3 -> Bus.R3
                           | R4 -> Bus.R4
                           | R5 -> Bus.R5
                           | R6 -> Bus.R6
                           | R7 -> Bus.R7
                           | R8 -> Bus.R8
                           | R9 -> Bus.R9
                           | R10 -> Bus.R10
                           | PC -> Bus.PC
                           | AC -> Bus.AC
                           | _ -> failwith "Internal Compiler Error")
                          |> Some }

        let operation = visitOperation operation

        let op =
            match maybeDest, operation with
            | Some MAR, { AMux = Some AMux.MBR } -> failwith "Can't write to MAR from MBR"
            | Some MAR, { ALU = Some _ } -> failwith "Writes to MAR can only be done through a copy from a register"
            | Some MAR, { ABus = Some aBus } ->
                Operation.combine
                    { operation with
                          ABus = None
                          BBus = Some aBus }
                    op
            | Some _, { ALU = Some _; Shifter = None } ->
                Operation.combine
                    { operation with
                          Shifter = Some Shifter.Noop }
                    op
            | Some _, { ALU = None; Shifter = None } ->
                Operation.combine
                    { operation with
                          Shifter = Some Shifter.Noop
                          ALU = Some ALU.ABus }
                    op
            | Some _, { ALU = None; Shifter = Some _ } -> Operation.combine { operation with ALU = Some ALU.ABus } op
            | _ -> Operation.combine operation op



        match (op, current) with
        | _ when Operation.canCombine op current -> Operation.combine op current
        | { ABus = aBus
            BBus = bBus
            ALU = (None
            | Some ALU.And
            | Some ALU.Add)
            MARWrite = (None
            | Some false) },
          _ when { op with BBus = aBus; ABus = bBus }
                 |> Operation.canCombine current ->
            { op with BBus = aBus; ABus = bBus }
            |> Operation.combine current
        | _,
          { ABus = aBus
            BBus = bBus
            ALU = (None
            | Some ALU.And
            | Some ALU.Add)
            MARWrite = (None
            | Some false) } when { current with
                                       BBus = aBus
                                       ABus = bBus }
                                 |> Operation.canCombine op ->
            { current with
                  BBus = aBus
                  ABus = bBus }
            |> Operation.combine op
        | _ -> failwithf "Failed to combine '%s' and '%s'" (op.ToString()) (current.ToString())


    instr
    |> List.fold (fun current instr ->
        match instr with
        | Operation (maybeDest, operation) -> visitOperation current maybeDest operation
        | Control (condition, address) ->
            match (current, condition) with
            | { Condition = Some _ }, _ -> failwith "Can't apply jumps to an instruction more than once"
            | _, None ->
                { current with
                      Condition = Some Cond.None
                      Address = Some address }
            | _, Some CondZero ->
                { current with
                      Condition = Some Cond.Zero
                      Address = Some address }
            | _, Some CondNegative ->
                { current with
                      Condition = Some Cond.Neg
                      Address = Some address }
        | MemoryAccess memoryAccess ->
            if current.MemoryAccess |> Option.isSome then
                failwith "Can't apply memory access twice"
            else
                { current with
                      MemoryAccess = Some memoryAccess }) op



let private visitLine line =
    match line with
    | LabelLine s -> [ Label s ]
    | InstructionLine instr -> [ visitInstructions instr |> AssemblyLine.Operation ]


let private visitFile file =
    file.Lines
    |> Seq.map visitLine
    |> Seq.reduce List.append

let parseAssembly string =
    string
    |> List.ofSeq
    |> tokenize
    |> parseFile
    |> visitFile
