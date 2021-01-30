module Micro16C.Frontend.Lex

open System



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
    | R0Keyword
    | R1Keyword
    | R2Keyword
    | R3Keyword
    | R4Keyword
    | R5Keyword
    | R6Keyword
    | R7Keyword
    | R8Keyword
    | R9Keyword
    | R10Keyword
    | ACKeyword
    | PCKeyword

type Token =
    { Offset: int
      Length: int
      Type: TokenType }

module Token =

    let identifier token =
        match token.Type with
        | Identifier s -> s
        | _ -> failwith "Internal Compiler Error: Token is not an identifier"

    let value token =
        match token.Type with
        | Literal value -> value
        | _ -> failwith "Internal Compiler Error: Token is not a literal"

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
            match this.Newlines
                  |> List.tryFindBack (snd >> (>) offset) with
            | None -> (-1, -1)
            | Some i -> i

        let prefix =
            sprintf "%d:%d: %s\n" (fst i + 2) (offset - snd i) message

        let endOffset =
            match this.Source.IndexOf(value = '\n', startIndex = snd i + 1) with
            | -1 -> String.length this.Source
            | value -> value

        prefix
        + sprintf "%4d | %s\n" (fst i + 2) this.Source.[(snd i + 1)..endOffset]

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

let tokenize (input: string) =

    let sourceObject = createSourceObject input

    let error (offset: int) (message: string) =
        sourceObject.emitError (ErrorTypeOffset offset) message
        |> Error

    let readNumber offset input =

        let (numberChars, numBase, skip) =
            match input with
            | '0' :: 'x' :: numberChars
            | '0' :: 'X' :: numberChars -> (numberChars, 16, 2)
            | '0' :: c :: numberChars when c >= '0' && c < '8' -> (c :: numberChars, 8, 1)
            | _ -> (input, 10, 0)

        let set =
            match numBase with
            | 8 ->
                Set
                    ([ '0'
                       '1'
                       '2'
                       '3'
                       '4'
                       '5'
                       '6'
                       '7' ])
            | 10 ->
                Set
                    ([ '0'
                       '1'
                       '2'
                       '3'
                       '4'
                       '5'
                       '6'
                       '7'
                       '8'
                       '9' ])
            | 16 ->
                Set
                    ([ '0'
                       '1'
                       '2'
                       '3'
                       '4'
                       '5'
                       '6'
                       '7'
                       '8'
                       '9'
                       'a'
                       'A'
                       'b'
                       'B'
                       'c'
                       'C'
                       'd'
                       'D'
                       'e'
                       'E'
                       'f'
                       'F' ])
            | _ -> failwithf "Internal Compiler Error: Invalid base %d" numBase

        let numberChars =
            numberChars
            |> List.takeWhile (fun c -> Set.contains c set)

        let rest =
            input
            |> List.skip (List.length numberChars + skip)

        let spelling =
            input
            |> List.take (List.length numberChars + skip)
            |> Array.ofList
            |> String

        let s = numberChars |> Array.ofList |> String

        let conversion (fromBase: int) (str: string) = Convert.ToInt16(str, fromBase)

        match numberChars with
        | [] ->
            (sprintf "Expected octal digits after integer literal with leading 0"
             |> error offset,
             rest)
        | _ ->
            try
                (conversion numBase s |> Ok, rest)
            with :? OverflowException ->
                (sprintf "Integer literal '%s' too large for type int" spelling
                 |> error offset,
                 rest)

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
        | [ c ] -> c |> int16 |> Ok
        | [] ->
            "Expected at least one character in character literal"
            |> error offset
        | '\\' :: 'x' :: hex ->
            let s = hex |> Array.ofList |> String

            try
                Convert.ToInt16(s, 16) |> Ok
            with
            | :? OverflowException ->
                sprintf "Hex literal '\x%s' does not fit into type int" s
                |> error offset
            | _ ->
                sprintf "Invalid hex literal '%s'" s
                |> error offset

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
            | _ ->
                let s = rest |> Array.ofList |> String

                try
                    Convert.ToInt16(s, 8) |> Ok
                with :? OverflowException ->
                    sprintf "Octal literal '\%s' does not fit into type int" s
                    |> error offset
        | [ '\\'; c ] ->
            match c with
            | ''' -> '\'' |> int16 |> Ok
            | '"' -> '"' |> int16 |> Ok
            | '?' -> '?' |> int16 |> Ok
            | '\\' -> '\\' |> int16 |> Ok
            | 'a' -> '\a' |> int16 |> Ok
            | 'b' -> '\b' |> int16 |> Ok
            | 'f' -> '\f' |> int16 |> Ok
            | 'n' -> '\n' |> int16 |> Ok
            | 'r' -> '\r' |> int16 |> Ok
            | 't' -> '\t' |> int16 |> Ok
            | 'v' -> '\v' |> int16 |> Ok
            | _ ->
                sprintf "Unknown escape character '%c'" c
                |> error offset
        | _ -> "Invalid character literal" |> error offset

    let rec readCharRec backslash (chars: string) offset input =
        match input with
        | ''' :: rest when not backslash -> (parseCharContent offset (chars |> List.ofSeq), rest)
        | '\\' as c :: rest -> readCharRec (not backslash) (chars + c.ToString()) (offset + 1) rest
        | [] ->
            (sprintf "Unterminated character literal"
             |> error offset,
             [])
        | c :: rest -> readCharRec false (chars + c.ToString()) (offset + 1) rest

    let readChar = readCharRec false ""

    let rec readBlockComment offset input =
        match input with
        | '*' :: '/' :: rest -> (Ok(), rest)
        | [] -> (error offset "Unterminated block comment", input)
        | _ :: rest -> readBlockComment (offset + 1) rest

    let tokenizeFirst offset input =

        match input with
        | c :: _ when Char.IsDigit c ->
            let num, rest = input |> readNumber offset
            let length = List.length input - List.length rest

            let num =
                num
                |> Result.map (fun num ->
                    { Offset = offset
                      Length = length
                      Type = Literal num })

            (Some num, rest, offset + length)
        | c :: _ when Char.IsLetter c || c = '_' ->
            let identifier, rest = input |> readIdentifier
            let length = List.length input - List.length rest

            let tokenType =
                match identifier with
                | "int" -> IntKeyword
                | "break" -> BreakKeyword
                | "continue" -> ContinueKeyword
                | "do" -> DoKeyword
                | "else" -> ElseKeyword
                | "for" -> ForKeyword
                | "while" -> WhileKeyword
                | "if" -> IfKeyword
                | "goto" -> GotoKeyword
                | "sizeof" -> SizeOfKeyword
                | "R0" -> R0Keyword
                | "R1" -> R1Keyword
                | "R2" -> R2Keyword
                | "R3" -> R3Keyword
                | "R4" -> R4Keyword
                | "R5" -> R5Keyword
                | "R6" -> R6Keyword
                | "R7" -> R7Keyword
                | "R8" -> R8Keyword
                | "R9" -> R9Keyword
                | "R10" -> R10Keyword
                | "AC" -> ACKeyword
                | "PC" -> PCKeyword
                | _ -> Identifier identifier

            ({ Offset = offset
               Length = length
               Type = tokenType }
             |> Ok
             |> Some,
             rest,
             offset + length)
        | c :: rest when Char.IsWhiteSpace c -> (None, rest, offset + 1)
        | [] -> (None, [], offset)
        | ''' :: rest ->
            let character, rest = rest |> readChar offset
            let length = List.length input - List.length rest

            let character =
                character
                |> Result.map (fun c ->
                    { Offset = offset
                      Length = length
                      Type = Literal c })

            (Some character, rest, offset + length)
        | '/' :: '/' :: rest ->
            let skipped = rest |> List.skipWhile ((<>) '\n')
            (None, skipped, offset + skipped.Length)
        | '/' :: '*' :: rest ->
            let (error, skipped) = readBlockComment offset rest

            match error with
            | Ok _ -> (None, skipped, offset + skipped.Length)
            | Error s1 -> (s1 |> Error |> Some, skipped, offset + skipped.Length)
        | '/' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = DivideAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '|' :: '|' :: rest ->


            ({ Offset = offset
               Length = 2
               Type = LogicOr }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '&' :: '&' :: rest ->

            ({ Offset = offset
               Length = 2
               Type = LogicAnd }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '=' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = Equal }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '!' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = NotEqual }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '<' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = LessThanOrEqual }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '>' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = GreaterThanOrEqual }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '+' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = PlusAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '-' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = MinusAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '/' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = DivideAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '*' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = MultiplyAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '%' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = ModuloAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '<' :: '<' :: '=' :: rest ->
            ({ Offset = offset
               Length = 3
               Type = ShiftLeftAssign }
             |> Ok
             |> Some,
             rest,
             offset + 3)
        | '>' :: '>' :: '=' :: rest ->
            ({ Offset = offset
               Length = 3
               Type = ShiftRightAssign }
             |> Ok
             |> Some,
             rest,
             offset + 3)
        | '&' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = BitAndAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '|' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = BitOrAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '^' :: '=' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = BitXorAssign }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '<' :: '<' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = ShiftLeft }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '>' :: '>' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = ShiftRight }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '+' :: '+' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = Increment }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '-' :: '-' :: rest ->
            ({ Offset = offset
               Length = 2
               Type = Decrement }
             |> Ok
             |> Some,
             rest,
             offset + 2)
        | '(' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = OpenParentheses }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | ')' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = CloseParentheses }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '{' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = OpenBrace }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '}' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = CloseBrace }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | ';' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = SemiColon }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | ',' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Comma }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '-' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Minus }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '~' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = BitWiseNegation }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '!' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = LogicalNegation }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '+' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Plus }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '*' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Asterisk }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '/' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Division }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '%' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Percent }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '&' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Ampersand }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '|' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = BitOr }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '^' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = BitXor }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '=' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Assignment }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '<' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = LessThan }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '>' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = GreaterThan }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | '?' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = QuestionMark }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | ':' :: rest ->
            ({ Offset = offset
               Length = 1
               Type = Colon }
             |> Ok
             |> Some,
             rest,
             offset + 1)
        | c :: rest ->
            let err =
                sprintf "Unexpected character '%c'" c
                |> error offset

            (err |> Some, rest, offset + 1)

    input
    |> List.ofSeq
    |> (fun x -> (x, 0))
    |> Seq.unfold (fun (chars, offset) ->
        if chars |> List.isEmpty then
            None
        else
            let token, chars, offset = tokenizeFirst offset chars
            Some(token, (chars, offset)))
    |> Seq.choose id
    |> ErrorHandling.foldResults
    |> Result.map (fun tokens ->
        { sourceObject with
              Tokens = tokens |> List.rev })
