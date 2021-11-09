[<RequireQualifiedAccess>]
module Micro16C

open System.IO
open Micro16C.CompileCode
open Micro16C.Backend

[<EntryPoint>]
let main argv =
    if argv |> Array.isEmpty then
        eprintfn "Expected file name as first parameter"
        -1
    else
        let fileName = argv.[0]
        let text = File.ReadAllText fileName

        match compile text |> Result.map Assembly.printAssembly with
        | Ok _ -> 0
        | Error s ->
            eprintfn $"%s{s}"
            -1
