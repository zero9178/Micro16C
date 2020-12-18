module Tests

open System
open Xunit
open FsUnit.Xunit

open Micro16CFrontend.Lex

let lexerOutput (input: string) =
    let mutable s = ""
    tokenizeRep (fun x -> s <- s + x) input |> ignore
    s

[<Fact>]
let ``Simple identifiers`` () =
    let tokens = tokenize "a"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 1
               Type = Identifier "a" } ]

    let tokens = tokenize "a2323"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 5
               Type = Identifier "a2323" } ]

    let tokens = tokenize "a34+343"
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

    let tokens = tokenize "a23_23"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 6
               Type = Identifier "a23_23" } ]

    let tokens = tokenize "_a2323"
    tokens |> should haveLength 1

    tokens
    |> should
        equal
           [ { Offset = 0
               Length = 6
               Type = Identifier "_a2323" } ]

[<Fact>]
let ``Recognizing keywords`` () =
    tokenize "int register break continue do else for while goto"
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
    tokenize "/*4234$34353534§$$343§$§$*/"
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
        tokenize "id\n
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
    let tokens = tokenize "3434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 3434))

    let tokens = tokenize "6521323"
    tokens |> should haveLength 1

    lexerOutput "6521323"
    |> should haveSubstring "Integer literal '6521323' too large for type int"

    let tokens = tokenize "0x3434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 0x3434))

    lexerOutput "0x6521323"
    |> should haveSubstring "Integer literal '0x6521323' too large for type int"

    let tokens = tokenize "0X3434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 0X3434))

    lexerOutput "0X6521323"
    |> should haveSubstring "Integer literal '0X6521323' too large for type int"

    let tokens = tokenize "03434"
    tokens |> should haveLength 1

    tokens.[0].Type
    |> should equal (Literal(int16 1820))

    lexerOutput "06521323"
    |> should haveSubstring "Integer literal '06521323' too large for type int"

[<Fact>]
let Operators () =
    tokenize "|| && == != <= >= += -= /= *= %= <<= >>= &= |= ^= << >> ++ -- ( ) { } ; - ~ ! + * / % & | ^ = < > ? :"
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
let miscellaneous () =
    lexerOutput "$"
    |> should haveSubstring "Unexpected character '$'"
