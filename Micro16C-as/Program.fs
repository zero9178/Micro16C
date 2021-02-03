// Learn more about F# at http://docs.microsoft.com/dotnet/fsharp

open System
open System.IO
open Micro16C.Backend

// Define a function to construct a message to print
let from whom = sprintf "from %s" whom

[<EntryPoint>]
let main argv =
    if argv |> Array.isEmpty then
        eprintfn "Expected file name as first parameter"
        -1
    else
        let machineCode =
            argv.[0]
            |> File.ReadAllText
            |> ParseAssembly.parseAssembly
            |> GenAssembly.genMachineCode ()
            |> Seq.cache

        machineCode |> Seq.iter (printfn "0x%08x")

        let path =
            (argv.[0]
             |> Array.ofSeq
             |> Array.splitAt
                 (argv.[0]
                  |> Array.ofSeq
                  |> Array.tryFindIndexBack ((=) '.')
                  |> Option.defaultValue (argv.[0] |> String.length))
             |> fst
             |> String)
            + ".o"

        machineCode
        |> Seq.map BitConverter.GetBytes
        |> Seq.collect (fun b -> if BitConverter.IsLittleEndian then b else b |> Array.rev)
        |> Array.ofSeq
        |> (fun x -> File.WriteAllBytes(path, x))

        0
