open System

open Micro16CFrontend

[<EntryPoint>]
let main argv =
    let tokens = Lex.tokenize "'\\''"
    printfn "%A" tokens
    0
