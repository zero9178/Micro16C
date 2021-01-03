open Micro16C.Backend
open Micro16C.Frontend
open Micro16C.MiddleEnd

let printModulePass title (irModule: IR.Module) =
    printf "%s\n%s\n" title (irModule.ToString())
    irModule

[<EntryPoint>]
let main argv =
    let result =
        Lex.tokenize "int r0 = 5;
        int r1 = 20;
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
        |> Result.map (printModulePass "Before optimizations:")
        |> Result.map Passes.deadCodeElimination
        |> Result.map Passes.instructionSimplify
        |> Result.map Passes.instructionCombine
        |> Result.map Passes.simplifyCFG
        |> Result.map Passes.analyzeAlloc
        |> Result.map Passes.analyzeDominance
        |> Result.map Passes.analyzeDominanceFrontiers
        |> Result.map Passes.mem2reg
        |> Result.map Passes.deadCodeElimination
        |> Result.map Passes.instructionSimplify
        |> Result.map Passes.instructionCombine
        |> Result.map Passes.simplifyCFG
        |> Result.map (printModulePass "End of optimizations:")
        |> Result.map Legalize.legalizeConstants
        |> Result.map Legalize.genPhiMoves
        |> Result.map (printModulePass "End of IR:")
        |> Result.map GenAssembly.genAssembly
        |> Result.map Assembly.printAssembly

    0
