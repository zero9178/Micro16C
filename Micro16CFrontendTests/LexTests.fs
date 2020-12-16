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
    tokenize "int signed unsigned register break continue do else for while goto"
    |> List.map (fun { Type = x } -> x)
    |> should
        equal
           [ IntKeyword
             SignedKeyword
             UnsignedKeyword
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
