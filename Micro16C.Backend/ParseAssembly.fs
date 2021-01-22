module Micro16C.Backend.ParseAssembly

open System

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
        match chars |> List.skipWhile Char.IsWhiteSpace with
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

<Operand> ::= <Register> | ONE | ZERO | MINUS ONE | OPEN_PARENTHESES <Operand> CLOSE_PARENTHESES

<Operation> ::= lsh OPEN_PARENTHESES <Operation> CLOSE_PARENTHESES
              | rhs OPEN_PARENTHESES <Operation> CLOSE_PARENTHESES
              | ~<Operand>
              | <Operand> + <Operand>
              | <Operand> & <Operand>
              | <Operand>

<Instruction> ::= [<Register> ARROW ] <Operation> { SEMI_COLON [<Register> ARROW ] <Operation> } [ SEMI_COLON ["if" ("N" | "Z")] "goto" DOT IDENTIFIER ]

<Label> ::= COLON IDENTIFIER

<File> ::= { <Instruction> | <Label> }

*)

type private Register =
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

type private Operand =
    | RegisterOperand of Register
    | One
    | Zero
    | NegOne
    | Parentheses of Operand

type private Operation =
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
    { Operations: (Register option * Operation) list
      Control: (Condition option * string) option }

type private Line =
    | InstructionLine of Instruction
    | LabelLine of string

type private File = { Lines: Line list }

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
    | Some t -> failwithf "Expected Register instead of '%A'" t

let rec private parseOperand tokens =
    match Seq.tryHead tokens with
    | Some Minus -> (NegOne, tokens |> Seq.tail |> require TokenType.One |> snd)
    | Some TokenType.One -> (One, tokens |> Seq.tail)
    | Some TokenType.Zero -> (Zero, tokens |> Seq.tail)
    | Some OpenParentheses ->
        let operand, tokens = tokens |> Seq.tail |> parseOperand
        let _, tokens = tokens |> require CloseParentheses
        (Parentheses operand, tokens)
    | _ ->
        let register, tokens = tokens |> parseRegister
        (RegisterOperand register, tokens)

let rec private parseOperation tokens =
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

        if s = "lhs" then (LeftShift operation, tokens) else (RightShift operation, tokens)
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
    let firstDestination, tokens =
        match Seq.take 2 tokens |> List.ofSeq with
        | [ _; Arrow ] ->
            let register, tokens = parseRegister tokens
            let _, tokens = require Arrow tokens
            (Some register, tokens)
        | _ -> (None, tokens)

    let firstOperation, tokens = parseOperation tokens

    let additional =
        Seq.unfold (fun tokens ->
            match Seq.take 2 tokens |> List.ofSeq with
            | [ SemiColon; Identifier "if" ]
            | [ SemiColon; Identifier "goto" ] -> None
            | SemiColon :: _ ->
                let tokens = tokens |> Seq.tail

                let destination, tokens =
                    match Seq.take 2 tokens |> List.ofSeq with
                    | [ _; Arrow ] ->
                        let register, tokens = parseRegister tokens
                        let _, tokens = require Arrow tokens
                        (Some register, tokens)
                    | _ -> (None, tokens)

                let operation, tokens = parseOperation tokens
                Some(((destination, operation), tokens), tokens)
            | _ -> None) tokens
        |> Seq.cache

    let tokens =
        additional
        |> Seq.map snd
        |> Seq.tryLast
        |> Option.defaultValue tokens

    let operations =
        (firstDestination, firstOperation)
        |> Seq.singleton
        |> Seq.append (additional |> Seq.map fst)

    let control, tokens =
        match Seq.tryHead tokens with
        | Some SemiColon ->
            let tokens = tokens |> Seq.tail

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
            | Some (Identifier s) -> ((condition, s) |> Some, tokens |> Seq.tail)
            | _ -> failwith "Expected identifier after 'goto'"
        | _ -> (None, tokens)

    ({ Operations = operations |> List.ofSeq
       Control = control },
     tokens)

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
                  Some(InstructionLine result, tokens)) tokens
          |> List.ofSeq }

let parseAssembly string =
    let tree =
        string |> List.ofSeq |> tokenize |> parseFile

    ()
