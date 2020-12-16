namespace Micro16CFrontend

open System

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
        | SignedKeyword
        | UnsignedKeyword
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

    let tokenize (input: string) =

        let emitError (input: string) (message: string) (offset: int) =
            let newLines =
                List.ofSeq input.[0..offset]
                |> List.indexed
                |> List.filter (snd >> (=) '\n')

            printfn
                "%d:%d: %s"
                (List.length newLines + 1)
                (match List.tryLast newLines with
                 | None -> offset
                 | Some ((last, _)) -> offset - last)
                message

            let startOffset =
                match List.tryLast newLines with
                | None -> 0
                | Some ((last, _)) -> last

            let endOffset =
                match input.IndexOf(value = '\n', startIndex = startOffset) with
                | -1 -> input.Length
                | value -> value

            printfn "%4d | %s" (newLines.Length + 1) input.[startOffset..endOffset]

        let error = emitError input

        let rec readNumberRec chars input =
            match input with
            | c :: rest when Char.IsDigit c -> readNumberRec (chars + Char.ToString c) rest
            | _ -> (chars |> int16, input)

        let readNumber = readNumberRec ""

        let rec readIdentifierRec chars input =
            match input with
            | c :: rest when Char.IsLetterOrDigit c -> readIdentifierRec (chars + Char.ToString c) rest
            | _ -> (chars, input)

        let readIdentifier = readIdentifierRec ""

        let parseCharContent charContent =
            match charContent with
            | c :: _ -> c |> int16
            | [] -> 0 |> int16 //TODO: Error
        //TODO: Escapes etc

        let rec readCharRec chars input =
            match input with
            | ''' :: _
            | [] -> (chars |> List.ofSeq |> parseCharContent, input)
            | c :: rest -> rest |> readCharRec (chars + c.ToString())

        let readChar = readCharRec ""

        let rec readBlockComment input =
            match input with
            | '*' :: '/' :: rest -> rest
            | [] -> input
            | _ :: rest -> readBlockComment rest

        let rec tokenizeFirst offset input =
            match input with
            | c :: _ when Char.IsDigit c ->
                let num, rest = input |> readNumber
                let length = List.length input - List.length rest

                { Offset = offset
                  Length = length
                  Type = Literal num }
                :: tokenizeFirst (offset + length) rest
            | c :: _ when Char.IsLetter c ->
                let identifier, rest = input |> readIdentifier
                let length = List.length input - List.length rest

                let tokenType =
                    match identifier with
                    | "int" -> IntKeyword
                    | "signed" -> SignedKeyword
                    | "unsigned" -> UnsignedKeyword
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
                let character, rest = rest |> readChar
                let length = input.Length - rest.Length

                { Offset = offset
                  Length = length
                  Type = Literal character }
                :: tokenizeFirst (offset + length) rest
            | '/' :: '/' :: rest ->
                let skipped = rest |> List.takeWhile ((<>) '/')
                tokenizeFirst (offset + skipped.Length) skipped
            | '/' :: '*' :: rest ->
                let skipped = readBlockComment rest
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
                error (sprintf "Unexpected character '%c'" c) offset
                tokenizeFirst (offset + 1) rest

        List.ofSeq input |> tokenizeFirst 0
