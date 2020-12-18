open System

open Micro16C.Frontend

[<EntryPoint>]
let main argv =
    let tokens = Lex.tokenize "'\\''"
    printfn "%A" tokens
    0
