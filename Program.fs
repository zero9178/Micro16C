open Micro16C.Frontend

[<EntryPoint>]
let main argv =
    Lex.tokenize "register(R0) int r0 = 5;
    int register(R1) r1 = 20;
    int mod;
    do
    {
        mod = r0 % r1;
        r0 = r1;
        r1 = mod;
    } while(mod != 0);
    register(R2) int r2 = r1;"
    |> Result.bind Parse.parse
    |> printfn "%A"

    0
