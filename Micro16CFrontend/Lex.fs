﻿namespace Micro16CFrontend

open System
open System.Globalization

module Lex =

    type TokenType =
        | Identifier of string
        | OpenParentheses
        | CloseParentheses
        | OpenBrace
        | CloseBrace
        | Literal of int16
        | SemiColon
        | Comma
        | Minus
        | BitWiseNegation
        | LogicalNegation
        | Plus
        | Asterisk
        | Division
        | Percent
        | LogicAnd
        | LogicOr
        | Ampersand
        | BitOr
        | BitXor
        | Equal
        | NotEqual
        | LessThan
        | LessThanOrEqual
        | GreaterThan
        | GreaterThanOrEqual
        | Assignment
        | PlusAssign
        | MinusAssign
        | DivideAssign
        | MultiplyAssign
        | ModuloAssign
        | ShiftLeftAssign
        | ShiftRightAssign
        | BitAndAssign
        | BitOrAssign
        | BitXorAssign
        | ShiftRight
        | ShiftLeft
        | Increment
        | Decrement
        | Colon
        | QuestionMark
        | IntKeyword
        | RegisterKeyword
        | BreakKeyword
        | ContinueKeyword
        | DoKeyword
        | ElseKeyword
        | ForKeyword
        | IfKeyword
        | WhileKeyword
        | OpenSquareBracket
        | CloseSquareBracket
        | GotoKeyword

    type Token =
        { Offset: int
          Length: int
          Type: TokenType }

    let tokenizeRep reporter (input: string) =

        let emitError (input: string) (offset: int) (message: string) =
            let newLines =
                List.ofSeq input.[0..offset]
                |> List.indexed
                |> List.filter (snd >> (=) '\n')

            sprintf "%d:%d: %s\n" (List.length newLines + 1)
                (match List.tryLast newLines with
                 | None -> offset
                 | Some (last, _) -> offset - last) message
            |> reporter

            let startOffset =
                match List.tryLast newLines with
                | None -> 0
                | Some (last, _) -> last

            let endOffset =
                match input.IndexOf(value = '\n', startIndex = startOffset) with
                | -1 -> input.Length
                | value -> value

            sprintf "%4d | %s\n" (List.length newLines + 1) input.[startOffset..endOffset]
            |> reporter

        let error = emitError input

        let readNumber offset input =
            let spelling = input |> Array.ofList |> String

            let conversion (fromBase: int) (str: string) = Convert.ToInt16(str, fromBase)

            let (input, convert) =
                match input with
                | '0' :: 'x' :: rest
                | '0' :: 'X' :: rest -> (rest, conversion 16)
                | '0' :: rest -> (rest, conversion 8)
                | _ -> (input, conversion 10)

            let numberChars = input |> List.takeWhile Char.IsDigit

            let rest =
                input |> List.skip (List.length numberChars)

            let s = numberChars |> Array.ofList |> String

            try
                (convert s, rest)
            with :? OverflowException ->
                sprintf "Integer literal '%s' too large for type int" spelling
                |> error offset

                (0s, rest)

        let readIdentifier input =
            let identifiers =
                input
                |> List.takeWhile (fun c -> Char.IsLetterOrDigit c || c = '_')

            (identifiers |> Array.ofList |> String, input |> List.skip (List.length identifiers))

        let parseCharContent offset charContent =
            match charContent with
            | [ '\\' ] ->
                "Expected escape character after '\\'"
                |> error offset

                0s
            | [ c ] -> c |> int16
            | [] ->
                "Expected at least one character in character literal"
                |> error offset

                0s
            | [ '\\'; c ] when c <> 'x' && not (Char.IsDigit c) ->
                match c with
                | ''' -> '\'' |> int16
                | '"' -> '"' |> int16
                | '?' -> '?' |> int16
                | '\\' -> '\\' |> int16
                | 'a' -> '\a' |> int16
                | 'b' -> '\b' |> int16
                | 'f' -> '\f' |> int16
                | 'n' -> '\n' |> int16
                | 'r' -> '\r' |> int16
                | 't' -> '\t' |> int16
                | 'v' -> '\v' |> int16
                | _ ->
                    sprintf "Unknown escape character '%c'" c
                    |> error offset

                    0s
            | '\\' :: 'x' :: hex ->
                let s = hex |> Array.ofList |> String

                try
                    Convert.ToInt16(s, 16)
                with
                | :? OverflowException ->
                    sprintf "Hex literal %s does not fit into type int" s
                    |> error offset

                    0s
                | _ ->
                    sprintf "Invalid hex literal '%s'" s
                    |> error offset

                    0s

            | '\\' :: rest when List.length rest > 0
                                && List.length rest <= 3
                                && List.forall Char.IsDigit rest ->
                match rest with
                | [ c; _; _ ]
                | [ _; c; _ ]
                | [ _; _; c ] when c >= '8' ->
                    sprintf "Invalid octal character %c" c
                    |> error offset

                    0s
                | _ -> Int16.Parse(rest |> Array.ofList |> String, NumberStyles.HexNumber)
            | _ ->
                "Invalid character literal" |> error offset
                0s

        let readChar offset input =
            let chars = input |> List.takeWhile ((<>) '\'')

            if List.length chars = List.length input then
                sprintf "Unterminated character literal"
                |> error offset

            (parseCharContent offset chars, input |> List.skip (List.length chars))

        let rec readBlockComment offset input =
            match input with
            | '*' :: '/' :: rest -> rest
            | [] ->
                error offset "Unterminated block comment"
                input
            | _ :: rest -> readBlockComment (offset + 1) rest

        let rec tokenizeFirst offset input =
            match input with
            | c :: _ when Char.IsDigit c ->
                let num, rest = input |> readNumber offset
                let length = List.length input - List.length rest

                { Offset = offset
                  Length = length
                  Type = Literal num }
                :: tokenizeFirst (offset + length) rest
            | c :: _ when Char.IsLetter c || c = '_' ->
                let identifier, rest = input |> readIdentifier
                let length = List.length input - List.length rest

                let tokenType =
                    match identifier with
                    | "int" -> IntKeyword
                    | "register" -> RegisterKeyword
                    | "break" -> BreakKeyword
                    | "continue" -> ContinueKeyword
                    | "do" -> DoKeyword
                    | "else" -> ElseKeyword
                    | "for" -> ForKeyword
                    | "while" -> WhileKeyword
                    | "goto" -> GotoKeyword
                    | _ -> Identifier identifier

                { Offset = offset
                  Length = length
                  Type = tokenType }
                :: tokenizeFirst (offset + length) rest
            | c :: rest when Char.IsWhiteSpace c -> tokenizeFirst (offset + 1) rest
            | [] -> []
            | ''' :: rest ->
                let character, rest = rest |> readChar offset
                let length = List.length input - List.length rest

                { Offset = offset
                  Length = length
                  Type = Literal character }
                :: tokenizeFirst (offset + length) rest
            | '/' :: '/' :: rest ->
                let skipped = rest |> List.skipWhile ((<>) '\n')
                tokenizeFirst (offset + skipped.Length) skipped
            | '/' :: '*' :: rest ->
                let skipped = readBlockComment offset rest
                tokenizeFirst (offset + skipped.Length) skipped
            | '/' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = DivideAssign }
                :: tokenizeFirst (offset + 2) rest
            | '|' :: '|' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = LogicOr }
                :: tokenizeFirst (offset + 2) rest
            | '&' :: '&' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = LogicAnd }
                :: tokenizeFirst (offset + 2) rest
            | '=' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = Equal }
                :: tokenizeFirst (offset + 2) rest
            | '!' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = NotEqual }
                :: tokenizeFirst (offset + 2) rest
            | '<' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = LessThanOrEqual }
                :: tokenizeFirst (offset + 2) rest
            | '>' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = GreaterThanOrEqual }
                :: tokenizeFirst (offset + 2) rest
            | '+' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = PlusAssign }
                :: tokenizeFirst (offset + 2) rest
            | '-' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = MinusAssign }
                :: tokenizeFirst (offset + 2) rest
            | '/' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = DivideAssign }
                :: tokenizeFirst (offset + 2) rest
            | '*' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = MultiplyAssign }
                :: tokenizeFirst (offset + 2) rest
            | '%' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = ModuloAssign }
                :: tokenizeFirst (offset + 2) rest
            | '<' :: '<' :: '=' :: rest ->
                { Offset = offset
                  Length = 3
                  Type = ShiftLeftAssign }
                :: tokenizeFirst (offset + 3) rest
            | '>' :: '>' :: '=' :: rest ->
                { Offset = offset
                  Length = 3
                  Type = ShiftRightAssign }
                :: tokenizeFirst (offset + 3) rest
            | '&' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = BitAndAssign }
                :: tokenizeFirst (offset + 2) rest
            | '|' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = BitOrAssign }
                :: tokenizeFirst (offset + 2) rest
            | '^' :: '=' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = BitXorAssign }
                :: tokenizeFirst (offset + 2) rest
            | '<' :: '<' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = ShiftLeft }
                :: tokenizeFirst (offset + 2) rest
            | '>' :: '>' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = ShiftRight }
                :: tokenizeFirst (offset + 2) rest
            | '+' :: '+' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = Increment }
                :: tokenizeFirst (offset + 2) rest
            | '-' :: '-' :: rest ->
                { Offset = offset
                  Length = 2
                  Type = Decrement }
                :: tokenizeFirst (offset + 2) rest
            | '(' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = OpenParentheses }
                :: tokenizeFirst (offset + 1) rest
            | ')' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = CloseParentheses }
                :: tokenizeFirst (offset + 1) rest
            | '{' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = OpenBrace }
                :: tokenizeFirst (offset + 1) rest
            | '}' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = CloseBrace }
                :: tokenizeFirst (offset + 1) rest
            | ';' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = SemiColon }
                :: tokenizeFirst (offset + 1) rest
            | '-' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Minus }
                :: tokenizeFirst (offset + 1) rest
            | '~' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = BitWiseNegation }
                :: tokenizeFirst (offset + 1) rest
            | '!' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = LogicalNegation }
                :: tokenizeFirst (offset + 1) rest
            | '+' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Plus }
                :: tokenizeFirst (offset + 1) rest
            | '*' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Asterisk }
                :: tokenizeFirst (offset + 1) rest
            | '/' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Division }
                :: tokenizeFirst (offset + 1) rest
            | '%' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Percent }
                :: tokenizeFirst (offset + 1) rest
            | '&' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Ampersand }
                :: tokenizeFirst (offset + 1) rest
            | '|' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = BitOr }
                :: tokenizeFirst (offset + 1) rest
            | '^' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = BitXor }
                :: tokenizeFirst (offset + 1) rest
            | '=' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Assignment }
                :: tokenizeFirst (offset + 1) rest
            | '<' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = LessThan }
                :: tokenizeFirst (offset + 1) rest
            | '>' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = GreaterThan }
                :: tokenizeFirst (offset + 1) rest
            | '?' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = QuestionMark }
                :: tokenizeFirst (offset + 1) rest
            | ':' :: rest ->
                { Offset = offset
                  Length = 1
                  Type = Colon }
                :: tokenizeFirst (offset + 1) rest
            | c :: rest ->
                sprintf "Unexpected character '%c'" c
                |> error offset

                tokenizeFirst (offset + 1) rest

        List.ofSeq input |> tokenizeFirst 0

    let tokenize = tokenizeRep Console.Write
