open Micro16C.Frontend
open Micro16C.MiddleEnd

[<EntryPoint>]
let main argv =
    Lex.tokenize "register(R0) int r0 = 5;
    int register(R1) r1 = 20;
    int mod;
    while(1)
    {
        mod = r0 % r1;
        r0 = r1;
        r1 = mod;
        if (mod == 0)
        {
            goto end;
        }
    }
    end:;
    register(R2) int r2 = r1;"
    |> Result.bind Parse.parse
    |> Result.bind Sema.analyse
    |> Result.map Codegen.codegen
    |> Result.map (fun x -> x.ToString())
    |> printf "%A"

    0
