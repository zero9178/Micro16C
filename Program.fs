open Micro16C.Backend
open Micro16C.Frontend
open Micro16C.MiddleEnd

let printModulePass title (irModule: IR.Module ref) =
    printf "%s\n%s\n" title ((!irModule) |> IR.Module.asText)
    irModule

[<EntryPoint>]
let main argv =
    let result =
        Lex.tokenize "int r0 = R0;
        int r1 = R1;
        int mod;
        do
        {
            mod = r0 % r1;
            r0 = r1;
            r1 = mod;
        }
        while(mod > 0);
        R2 = r1;"
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
        |> Result.map (printModulePass "Before jump threading:")
        |> Result.map Passes.jumpThreading
        |> Result.map Passes.instructionSimplify
        |> Result.map Passes.instructionCombine
        |> Result.map Passes.simplifyCFG
        |> Result.map Passes.removeRedundantLoadStores
        //|> Result.map (printModulePass "End of optimizations:")
        |> Result.map Legalize.legalizeConstants
        |> Result.map Legalize.destroyCriticalEdges
        |> Result.map Legalize.genPhiMoves
        //|> Result.map (printModulePass "End of IR:")
        |> Result.map Passes.numberAll
        |> Result.map Passes.analyzeLifetimes
        |> Result.map RegisterAllocator.allocateRegisters
        |> Result.map GenAssembly.genAssembly
        |> Result.map GenAssembly.removeRedundantLabels
        |> Result.map Assembly.printAssembly

    0
