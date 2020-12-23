module LexTests

open System
open Xunit
open FsUnit.Xunit

open Micro16C.Frontend.Lex

let private lexerOutput (input: string) =
    match tokenize input with
    | Ok _ -> ""
    | Error s -> s

let private lexer (input: string) =
    match tokenize input with
    | Ok { Tokens = tokens } -> tokens
    | Error s -> failwith s

[<Fact>]
let ``Simple identifiers`` () =
    let tokens = lexer "a"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 1
               Type = Identifier "a" } ]

    let tokens = lexer "a2323"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 5
               Type = Identifier "a2323" } ]

    let tokens = lexer "a34+343"
    tokens |> should haveLength 3

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 3
               Type = Identifier "a34" }
             { Offset = 3; Length = 1; Type = Plus }
             { Offset = 4
               Length = 3
               Type = 343 |> int16 |> Literal } ]

    let tokens = lexer "a23_23"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 6
               Type = Identifier "a23_23" } ]

    let tokens = lexer "_a2323"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 6
               Type = Identifier "_a2323" } ]

[<Fact>]
let ``Recognizing keywords`` () =
    lexer "int register break continue do else for while goto"
    |> List.map (fun { Type = x } -> x)
    |> should
        equal
           [ IntKeyword
             RegisterKeyword
             BreakKeyword
             ContinueKeyword
             DoKeyword
             ElseKeyword
             ForKeyword
             WhileKeyword
             GotoKeyword ]

[<Fact>]
let ``Block comment`` () =
    lexer "/*4234$34353534§$$343§$§$*/"
    |> should haveLength 0

    lexerOutput "/*4234$34353534§$$343§$§$*/"
    |> should be EmptyString

    lexerOutput "/*4234$"
    |> should haveSubstring "Unterminated block comment"

[<Fact>]
let ``Line comment`` () =

    lexerOutput "id\n
    //4234$34353534§$$343§$§$\n
    f"
    |> should be EmptyString

    let tokens =
        lexer "id\n
    //4234$34353534§$$343§$§$\n
    f"

    tokens |> should haveLength 2

    tokens
    |> List.forall (fun { Type = x } ->
        match x with
        | Identifier _ -> true
        | _ -> false)
    |> Assert.True

[<Fact>]
let Integers () =

    lexerOutput "0" |> should be EmptyString

    let tokens = lexer "0x3434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 0x3434))

    let tokens = lexer "0"
    tokens |> should haveLength 1

    tokens.[0].Type |> should equal (Literal(int16 0))

    let tokens = lexer "3434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 3434))

    lexerOutput "6521323"
    |> should haveSubstring "Integer literal '6521323' too large for type int"

    lexerOutput "0x6521323"
    |> should haveSubstring "Integer literal '0x6521323' too large for type int"

    let tokens = lexer "0X3434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 0X3434))

    lexerOutput "0X6521323"
    |> should haveSubstring "Integer literal '0X6521323' too large for type int"

    let tokens = lexer "03434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 1820))

    lexerOutput "06521323"
    |> should haveSubstring "Integer literal '06521323' too large for type int"

[<Fact>]
let Operators () =
    lexer "|| && == != <= >= += -= /= *= %= <<= >>= &= |= ^= << >> ++ -- ( ) { } ; - ~ ! + * / % & | ^ = < > ? :"
    |> List.map (fun { Type = x } -> x)
    |> should
        equal
           [ LogicOr
             LogicAnd
             Equal
             NotEqual
             LessThanOrEqual
             GreaterThanOrEqual
             PlusAssign
             MinusAssign
             DivideAssign
             MultiplyAssign
             ModuloAssign
             ShiftLeftAssign
             ShiftRightAssign
             BitAndAssign
             BitOrAssign
             BitXorAssign
             ShiftLeft
             ShiftRight
             Increment
             Decrement
             OpenParentheses
             CloseParentheses
             OpenBrace
             CloseBrace
             SemiColon
             Minus
             BitWiseNegation
             LogicalNegation
             Plus
             Asterisk
             Division
             Percent
             Ampersand
             BitOr
             BitXor
             Assignment
             LessThan
             GreaterThan
             QuestionMark
             Colon ]

[<Fact>]
let Miscellaneous () =
    lexerOutput "$"
    |> should haveSubstring "Unexpected character '$'"

[<Fact>]
let ``Character literals`` () =
    lexer "'a'"
    |> should
        equal
           [ { Offset = 0
               Type = 'a' |> int16 |> Literal
               Length = 3 } ]

    lexerOutput "''"
    |> should haveSubstring "Expected at least one character in character literal"

    lexerOutput "'"
    |> should haveSubstring "Unterminated character literal"

    lexer "'\\''"
    |> should
        equal
           [ { Offset = 0
               Type = '\'' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\\"'"
    |> should
        equal
           [ { Offset = 0
               Type = '"' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\?'"
    |> should
        equal
           [ { Offset = 0
               Type = '?' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\\\'"
    |> should
        equal
           [ { Offset = 0
               Type = '\\' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\a'"
    |> should
        equal
           [ { Offset = 0
               Type = '\a' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\b'"
    |> should
        equal
           [ { Offset = 0
               Type = '\b' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\f'"
    |> should
        equal
           [ { Offset = 0
               Type = '\f' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\n'"
    |> should
        equal
           [ { Offset = 0
               Type = '\n' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\r'"
    |> should
        equal
           [ { Offset = 0
               Type = '\r' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\t'"
    |> should
        equal
           [ { Offset = 0
               Type = '\t' |> int16 |> Literal
               Length = 4 } ]

    lexer "'\\v'"
    |> should
        equal
           [ { Offset = 0
               Type = '\v' |> int16 |> Literal
               Length = 4 } ]

    lexerOutput "'\\$'"
    |> should haveSubstring "Unknown escape character '$'"

    lexer "'\\x60'"
    |> should
        equal
           [ { Offset = 0
               Type = Literal 0x60s
               Length = 6 } ]

    lexerOutput "'\\x3434343434'"
    |> should haveSubstring "Hex literal '\\x3434343434' does not fit into type int"


    lexer "'\\7'"
    |> should
        equal
           [ { Offset = 0
               Type = Literal 7s
               Length = 4 } ]

    lexer "'\\07'"
    |> should
        equal
           [ { Offset = 0
               Type = Literal 7s
               Length = 5 } ]

    lexer "'\\007'"
    |> should
        equal
           [ { Offset = 0
               Type = Literal 7s
               Length = 6 } ]


    lexerOutput "'\\0007'"
    |> should haveSubstring "Invalid character literal"

    lexerOutput "'\\8'"
    |> should haveSubstring "Invalid octal character '8'"

    lexerOutput "'\\08'"
    |> should haveSubstring "Invalid octal character '8'"

    lexerOutput "'\\008'"
    |> should haveSubstring "Invalid octal character '8'"
