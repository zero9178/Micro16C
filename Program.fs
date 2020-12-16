open System

open Micro16CFrontend

[<EntryPoint>]
let main argv =
    let tokens = Lex.tokenize "int i = 3;$ 3 /= 5;"
    printfn "%A" tokens
    0
