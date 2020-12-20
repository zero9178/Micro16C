module Micro16C.Frontend.Lex

open System
open System.Globalization



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
    | SizeOfKeyword
    | OpenSquareBracket
    | CloseSquareBracket
    | GotoKeyword

type Token =
    { Offset: int
      Length: int
      Type: TokenType }

type ErrorType =
    | ErrorTypeOffset of int
    | ErrorTypeToken of Token
    | ErrorTypeEnd

type SourceObject =
    { Source: string
      Newlines: (int * int) list
      Tokens: Token list }

    member this.emitError (offset: ErrorType) message =
        let offset =
            match offset with
            | ErrorTypeOffset i -> i
            | ErrorTypeToken token -> token.Offset
            | ErrorTypeEnd -> String.length this.Source

        let i =
            match this.Newlines |> List.tryFind (snd >> (<) offset) with
            | None -> (0, 0)
            | Some i -> i

        let prefix =
            sprintf "%d:%d: %s\n" (fst i + 1) (offset - snd i + 1) message

        let endOffset =
            match this.Source.IndexOf(value = '\n', startIndex = snd i) with
            | -1 -> String.length this.Source
            | value -> value

        prefix
        + sprintf "%4d | %s\n" (List.length this.Newlines + 1) this.Source.[(snd i)..endOffset]

let createSourceObject (input: string) =
    let newLines =
        List.ofSeq input
        |> List.indexed
        |> List.filter (snd >> (=) '\n')
        |> List.map fst
        |> List.indexed

    { Source = input
      Newlines = newLines
      Tokens = [] }

let tokenizeRep reporter (input: string) =

    let sourceObject = createSourceObject input

    let error (offset: int) =
        sourceObject.emitError (ErrorTypeOffset offset)
        >> reporter

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
        | '\\' :: 'x' :: hex ->
            let s = hex |> Array.ofList |> String

            try
                Convert.ToInt16(s, 16)
            with
            | :? OverflowException ->
                sprintf "Hex literal '\x%s' does not fit into type int" s
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
            | [ c; _ ]
            | [ c ]
            | [ _; c; _ ]
            | [ _; c ]
            | [ _; _; c ] when c >= '8' ->
                sprintf "Invalid octal character '%c'" c
                |> error offset

                0s
            | _ ->
                let s = rest |> Array.ofList |> String

                try
                    Convert.ToInt16(s, 8)
                with :? OverflowException ->
                    sprintf "Octal literal '\%s' does not fit into type int" s
                    |> error offset

                    0s
        | [ '\\'; c ] ->
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
        | _ ->
            "Invalid character literal" |> error offset
            0s

    let rec readCharRec backslash (chars: string) offset input =
        match input with
        | ''' :: rest when not backslash -> (parseCharContent offset (chars |> List.ofSeq), rest)
        | '\\' as c :: rest -> readCharRec (not backslash) (chars + c.ToString()) (offset + 1) rest
        | [] ->
            sprintf "Unterminated character literal"
            |> error offset

            (0s, [])
        | c :: rest -> readCharRec false (chars + c.ToString()) (offset + 1) rest

    let readChar = readCharRec false ""

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
                | "sizeof" -> SizeOfKeyword
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

    let tokens = input |> List.ofSeq |> tokenizeFirst 0
    { sourceObject with Tokens = tokens }

let tokenize = tokenizeRep Console.Error.Write
