open System

open Micro16C.Frontend

[<EntryPoint>]
let main argv =
    Lex.tokenize "int i = 5 + 3;"
    |> Parse.parse
    |> printfn "%A"

    0
