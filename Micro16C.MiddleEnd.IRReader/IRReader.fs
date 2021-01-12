module Micro16C.MiddleEnd.IRReader

open System
open Micro16C.MiddleEnd.IR

(*
Lexer Tokens:

CONSTANT = "\d+"
IDENTIFIER = ([^\W\d]|[.$])[\w.$]*
PERCENT = "%"
EQUALS = "="
LESS_THAN = "<"
ARROW = "->"
OPEN_PARENTHESES = "("
CLOSE_PARENTHESES = ")"
COLON = ":"
COMMA = ","
COMMENT = ";.*"

*)

type private TokenType =
    | Constant of int16
    | Identifier of string
    | Percent
    | Equals
    | LessThan
    | Arrow
    | OpenParentheses
    | CloseParentheses
    | Colon
    | Comma

let rec private tokenize chars =
    match chars with
    | [] -> []
    | c :: _ when Char.IsDigit c ->
        let number = chars |> List.takeWhile Char.IsDigit
        let chars = chars |> List.skip (List.length number)

        (number
         |> Array.ofList
         |> String
         |> int16
         |> Constant)
        :: tokenize chars
    | c :: _ when Char.IsLetter c || c = '_' || c = '$' || c = '.' ->
        let identifier =
            chars
            |> List.takeWhile (fun c ->
                Char.IsLetterOrDigit c
                || c = '_'
                || c = '$'
                || c = '.')

        let chars =
            chars |> List.skip (List.length identifier)

        (identifier |> Array.ofList |> String |> Identifier)
        :: tokenize chars
    | '-' :: '>' :: rest -> Arrow :: tokenize rest
    | '%' :: rest -> Percent :: tokenize rest
    | '=' :: rest -> Equals :: tokenize rest
    | '<' :: rest -> LessThan :: tokenize rest
    | '(' :: rest -> OpenParentheses :: tokenize rest
    | ')' :: rest -> CloseParentheses :: tokenize rest
    | ':' :: rest -> Colon :: tokenize rest
    | ',' :: rest -> Comma :: tokenize rest
    | ';' :: rest -> rest |> List.skipWhile ((<>) '\n') |> tokenize
    | c :: rest when Char.IsWhiteSpace c -> tokenize rest
    | c :: _ -> failwithf "Invalid character '%c'" c

(*
Grammar:

<Value> ::= '%' CONSTANT | '%' IDENTIFIER

<Operand> ::= <Value> | CONSTANT | "undef" | "R0" | "R1" | "R2" | "R3" | "R4" | "R5" | "R6" | "R7" | "R8" | "R9" | "R10" | "AC" | "PC"

<Label> ::= <Value> ':'

<BasicBlock> ::= <Label> { <Instructions> }

<Instructions> ::= <Value> '=' "alloca"
                 | <Value> '=' "add" <Operand> <Operand>
                 | <Value> '=' "and" <Operand> <Operand>
                 | <Value> '=' "not" <Operand>
                 | <Value> '=' "shl" <Operand>
                 | <Value> '=' "shr" <Operand>
                 | <Value> '=' "load" <Operand>
                 | <Value> '=' "copy" <Operand>
                 | <Value> '=' "phi" '(' <Operand> ',' <Value> ')' { '(' <Operand> ',' <Value> ')' }
                 | "goto" <Value>
                 | "br" '<' "0" <Operand> <Value> <Value>
                 | "br" '=' "0" <Operand> <Value> <Value>
                 | "store" <Operand> '->' <Operand>

<Module> ::= { <BasicBlock> }
*)

type private Value =
    | UnnamedValue of int16
    | NamedValue of string

type private Operand =
    | ValueOperand of Value
    | Constant of int16
    | Undef
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
    | AC
    | PC

type private Instruction =
    | Alloca of Value
    | Goto of Value
    | Add of Value * Operand * Operand
    | And of Value * Operand * Operand
    | Not of Value * Operand
    | Shl of Value * Operand
    | Shr of Value * Operand
    | Load of Value * Operand
    | Copy of Value * Operand
    | BrNeg of Operand * Value * Value
    | BrZero of Operand * Value * Value
    | Store of Operand * Operand
    | Phi of Value * (Operand * Value) list

type private BasicBlock =
    { Label: Value
      Instructions: Instruction list }

let private require tokenType =
    function
    | [] -> failwithf "Expected '%A'" tokenType
    | t :: rest when t = tokenType -> (t, rest)
    | t :: _ -> failwithf "Expected '%A' instead of '%A'" tokenType t

let private parseValue tokens =
    match tokens |> require Percent |> snd with
    | TokenType.Constant c :: rest -> (UnnamedValue c, rest)
    | TokenType.Identifier s :: rest -> (NamedValue s, rest)
    | _ -> failwith "Expected Identifier or Constant after '%'"

let private parseOperand =
    function
    | Percent :: _ as tokens ->
        let value, tokens = parseValue tokens
        (value |> ValueOperand, tokens)
    | TokenType.Constant c :: tokens -> (Constant c, tokens)
    | Identifier "undef" :: tokens -> (Undef, tokens)
    | Identifier "R0" :: tokens -> (R0, tokens)
    | Identifier "R1" :: tokens -> (R1, tokens)
    | Identifier "R2" :: tokens -> (R2, tokens)
    | Identifier "R3" :: tokens -> (R3, tokens)
    | Identifier "R4" :: tokens -> (R4, tokens)
    | Identifier "R5" :: tokens -> (R5, tokens)
    | Identifier "R6" :: tokens -> (R6, tokens)
    | Identifier "R7" :: tokens -> (R7, tokens)
    | Identifier "R8" :: tokens -> (R8, tokens)
    | Identifier "R9" :: tokens -> (R9, tokens)
    | Identifier "R10" :: tokens -> (R10, tokens)
    | Identifier "AC" :: tokens -> (AC, tokens)
    | Identifier "PC" :: tokens -> (PC, tokens)
    | t :: _ -> failwithf "Expected Operand instead of %A" t
    | _ -> failwith "Expected Operand"

let private parseBasicBlock tokens =
    let value, tokens = parseValue tokens
    let _, tokens = require Colon tokens

    let rec parseInstruction tokens =
        match tokens with
        | Percent :: TokenType.Constant _ :: Colon :: _
        | Percent :: Identifier _ :: Colon :: _ -> ([], tokens)
        | Percent :: _ ->
            let value, tokens = parseValue tokens
            let _, tokens = require Equals tokens

            match tokens with
            | Identifier "alloca" :: tokens ->
                let result, tokens = parseInstruction tokens
                (Alloca value :: result, tokens)
            | Identifier "add" :: tokens ->
                let lhs, tokens = parseOperand tokens
                let rhs, tokens = parseOperand tokens
                let result, tokens = parseInstruction tokens
                (Add(value, lhs, rhs) :: result, tokens)
            | Identifier "and" :: tokens ->
                let lhs, tokens = parseOperand tokens
                let rhs, tokens = parseOperand tokens
                let result, tokens = parseInstruction tokens
                (And(value, lhs, rhs) :: result, tokens)
            | Identifier "not" :: tokens ->
                let op, tokens = parseOperand tokens
                let result, tokens = parseInstruction tokens
                (Not(value, op) :: result, tokens)
            | Identifier "shl" :: tokens ->
                let op, tokens = parseOperand tokens
                let result, tokens = parseInstruction tokens
                (Shl(value, op) :: result, tokens)
            | Identifier "shr" :: tokens ->
                let op, tokens = parseOperand tokens
                let result, tokens = parseInstruction tokens
                (Shr(value, op) :: result, tokens)
            | Identifier "load" :: tokens ->
                let op, tokens = parseOperand tokens
                let result, tokens = parseInstruction tokens
                (Load(value, op) :: result, tokens)
            | Identifier "copy" :: tokens ->
                let op, tokens = parseOperand tokens
                let result, tokens = parseInstruction tokens
                (Copy(value, op) :: result, tokens)
            | Identifier "phi" :: tokens ->
                let _, tokens = require OpenParentheses tokens
                let fstOp, tokens = parseOperand tokens
                let _, tokens = require Comma tokens
                let fstValue, tokens = parseValue tokens
                let _, tokens = require CloseParentheses tokens

                let list =
                    Seq.unfold (fun tokens ->
                        if tokens |> List.tryHead <> Some OpenParentheses then
                            None
                        else
                            let tokens = tokens |> List.tail
                            let op, tokens = parseOperand tokens
                            let _, tokens = require Comma tokens
                            let value, tokens = parseValue tokens
                            let _, tokens = require CloseParentheses tokens
                            Some(((op, value), tokens), tokens)) tokens
                    |> List.ofSeq

                let incoming, tokens =
                    (list |> List.map fst,
                     list
                     |> List.tryLast
                     |> Option.map snd
                     |> Option.defaultValue tokens)

                let result, tokens = parseInstruction tokens

                (Phi(value, (fstOp, fstValue) :: incoming)
                 :: result,
                 tokens)
            | t :: _ -> failwithf "Expected instruction instead of %A after '='" t
            | [] -> failwith "Expected instruction after '='"
        | Identifier "goto" :: tokens ->
            let value, tokens = parseValue tokens
            let result, tokens = parseInstruction tokens
            (Goto value :: result, tokens)
        | Identifier "br" :: LessThan :: tokens ->
            let _, tokens = require (TokenType.Constant 0s) tokens
            let cond, tokens = parseOperand tokens
            let trueBranch, tokens = parseValue tokens
            let falseBranch, tokens = parseValue tokens
            let result, tokens = parseInstruction tokens
            (BrNeg(cond, trueBranch, falseBranch) :: result, tokens)
        | Identifier "br" :: tokens ->
            let cond, tokens = parseOperand tokens

            let kind, tokens =
                match tokens with
                | LessThan :: tokens -> (CondBrKind.Negative, tokens)
                | Equals :: tokens -> (CondBrKind.Zero, tokens)
                | t :: _ -> failwithf "Expected '=' or '<' instead of %A" t
                | [] -> failwith "Expected '=' or '<'"

            let _, tokens = require (TokenType.Constant 0s) tokens
            let trueBranch, tokens = parseValue tokens
            let falseBranch, tokens = parseValue tokens
            let result, tokens = parseInstruction tokens

            match kind with
            | CondBrKind.Negative -> (BrNeg(cond, trueBranch, falseBranch) :: result, tokens)
            | CondBrKind.Zero -> (BrZero(cond, trueBranch, falseBranch) :: result, tokens)
        | Identifier "store" :: tokens ->
            let value, tokens = parseOperand tokens
            let _, tokens = require Arrow tokens
            let destination, tokens = parseOperand tokens
            let result, tokens = parseInstruction tokens
            (Store(value, destination) :: result, tokens)
        | t :: _ -> failwithf "Unexpected token %A" t
        | [] -> ([], [])

    let instructions, tokens = parseInstruction tokens

    ({ Label = value
       Instructions = instructions },
     tokens)


let private parseModule =
    Seq.unfold (fun tokens -> if List.isEmpty tokens then None else parseBasicBlock tokens |> Some)

type private ValueState =
    | Found of IR.Value ref
    | Pending of (IR.Value ref * int) list

let fromString text: Module ref =

    let irModule = ref Module.Default
    let builder = Builder.fromModule irModule

    let valueToName =
        function
        | UnnamedValue _ -> ""
        | NamedValue s -> s

    let defineValue valueAst value map =
        match map |> Map.tryFind valueAst with
        | None -> map |> Map.add valueAst (Found value)
        | Some (Found _) -> failwithf "Redefinition of %A" valueAst
        | Some (Pending waiting) ->
            waiting
            |> List.iter (fun (ref, index) -> Value.setOperand index value ref)

            map |> Map.add valueAst (Found value)

    let assignOperand value index operandAst map =
        match operandAst with
        | Undef -> map
        | R0 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R0)

            map
        | R1 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R1)

            map
        | R2 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R2)

            map
        | R3 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R3)

            map
        | R4 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R4)

            map
        | R5 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R5)

            map
        | R6 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R6)

            map
        | R7 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R7)

            map
        | R8 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R8)

            map
        | R9 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R9)

            map
        | R10 ->
            value
            |> Value.setOperand index (Builder.createRegister Register.R10)

            map
        | PC ->
            value
            |> Value.setOperand index (Builder.createRegister Register.PC)

            map
        | AC ->
            value
            |> Value.setOperand index (Builder.createRegister Register.AC)

            map
        | Constant c ->
            value
            |> Value.setOperand index (Builder.createConstant c)

            map
        | ValueOperand valueAst ->
            match map |> Map.tryFind valueAst with
            | (Some (Found ref)) ->
                value |> Value.setOperand index ref
                map
            | None ->
                map
                |> Map.add valueAst (Pending [ (value, index) ])
            | Some (Pending list) ->
                map
                |> Map.add valueAst (Pending((value, index) :: list))


    text
    |> List.ofSeq
    |> tokenize
    |> parseModule
    |> Seq.fold (fun map bb ->
        let block =
            builder
            |> Builder.createBasicBlock (valueToName bb.Label)

        let builder =
            builder |> Builder.setInsertBlock (Some block)

        let map = defineValue bb.Label block map

        bb.Instructions
        |> List.fold (fun map instr ->
            match instr with
            | Alloca value ->
                let alloca =
                    builder
                    |> Builder.createNamedAlloca (valueToName value)
                    |> fst

                defineValue value alloca map
            | Goto value ->
                let goto =
                    builder
                    |> Builder.createGoto Value.UndefValue
                    |> fst

                assignOperand goto 0 (ValueOperand value) map
            | And (value, lhs, rhs)
            | Add (value, lhs, rhs) ->

                let kind =
                    match instr with
                    | And _ -> BinaryKind.And
                    | Add _ -> BinaryKind.Add
                    | _ -> failwith "Not possible"

                let bin =
                    builder
                    |> Builder.createNamedBinary (valueToName value) Value.UndefValue kind Value.UndefValue
                    |> fst

                let map = assignOperand bin 0 lhs map
                let map = assignOperand bin 1 rhs map
                defineValue value bin map
            | Not (value, operand)
            | Shl (value, operand)
            | Shr (value, operand) ->
                let kind =
                    match instr with
                    | Not _ -> UnaryKind.Not
                    | Shl _ -> UnaryKind.Shl
                    | Shr _ -> UnaryKind.Shr
                    | _ -> failwith "Not possible"

                let unary =
                    builder
                    |> Builder.createNamedUnary (valueToName value) kind Value.UndefValue
                    |> fst

                let map = assignOperand unary 0 operand map
                defineValue value unary map
            | Load (value, operand) ->
                let load =
                    builder
                    |> Builder.createNamedLoad (valueToName value) Value.UndefValue
                    |> fst

                let map = assignOperand load 0 operand map
                defineValue value load map
            | Copy (value, operand) ->
                let copy =
                    builder
                    |> Builder.createNamedCopy (valueToName value) Value.UndefValue
                    |> fst

                let map = assignOperand copy 0 operand map
                defineValue value copy map
            | BrZero (cond, trueBranch, falseBranch)
            | BrNeg (cond, trueBranch, falseBranch) ->
                let kind =
                    match instr with
                    | BrZero _ -> CondBrKind.Zero
                    | BrNeg _ -> CondBrKind.Negative
                    | _ -> failwith "Not possible"

                let condBr =
                    builder
                    |> Builder.createCondBr kind Value.UndefValue Value.UndefValue Value.UndefValue
                    |> fst

                let map = assignOperand condBr 0 cond map

                let map =
                    assignOperand condBr 1 (ValueOperand trueBranch) map

                assignOperand condBr 2 (ValueOperand falseBranch) map
            | Store (value, destination) ->
                let store =
                    builder
                    |> Builder.createStore Value.UndefValue Value.UndefValue
                    |> fst

                let map = assignOperand store 1 value map
                assignOperand store 0 destination map
            | Phi (value, list) ->
                let phi =
                    builder
                    |> Builder.createNamedPhi
                        (valueToName value)
                           (List.init (List.length list) (fun _ -> (Value.UndefValue, Value.UndefValue)))
                    |> fst

                let map =
                    list
                    |> List.fold (fun (map, i) (operand, value) ->
                        let map = assignOperand phi (2 * i) operand map

                        let map =
                            assignOperand phi (2 * i + 1) (ValueOperand value) map

                        (map, i + 1)) (map, 0)
                    |> fst

                defineValue value phi map) map) Map.empty
    |> Map.filter (fun _ ->
        function
        | Found _ -> false
        | _ -> true)
    |> Some
    |> Option.filter (Map.isEmpty >> not)
    |> Option.iter (fun x ->
        x
        |> Map.iter (fun _ -> (eprintfn "Undefined reference to %A"))

        failwith "Linkage failed")


    irModule
