open Micro16C.Frontend

[<EntryPoint>]
let main argv =
    Lex.tokenize "int r0 = 5;
    int r1 = 20;
    int mod;
    do
    {
        mod = r0 % r1;
        r0 = r1;
        r1 = mod;
    } while(mod != 0);"
    |> Result.map Parse.parse
    |> printfn "%A"

    0
