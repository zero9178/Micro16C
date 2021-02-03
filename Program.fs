[<RequireQualifiedAccess>]
module Micro16C

open System.IO
open Micro16C.Backend
open Micro16C.Frontend
open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.PassManager

let private debugModulePasses title =
    { Pass =
          (fun irModule ->
#if DEBUG
              printf "%s\n%s\n" title ((!irModule) |> IR.Module.asText)
#endif

              irModule)

      DependsOn = []
      Invalidates = [] }


let compile text =

    let passManager =
        PassManager.Default()
        |> PassManager.registerAnalysis Passes.analyzeAllocPass
        |> PassManager.registerAnalysis Passes.analyzeDominancePass
        |> PassManager.registerAnalysis Passes.analyzeDominanceFrontiersPass
        |> PassManager.registerAnalysis Passes.analyzeBitsReadPass
        |> PassManager.registerAnalysis Passes.analyzeLivenessPass
        |> PassManager.registerAnalysis RegisterAllocator.allocateRegistersPass
        |> PassManager.queueTransform (debugModulePasses "Before optimizations:")
        |> PassManager.queueTransform Passes.removeUnreachableBlocksPass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform Passes.mem2regPass
        |> PassManager.queueTransform Passes.constantPropagationPass
        |> PassManager.queueTransform Passes.deadCodeEliminationPass
        |> PassManager.queueTransform Passes.instructionSimplifyPass
        |> PassManager.queueTransform Passes.instructionCombinePass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform Passes.jumpThreadingPass
        |> PassManager.queueTransform Passes.constantPropagationPass
        |> PassManager.queueTransform Passes.deadCodeEliminationPass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform Legalize.legalizeInstructionsPass
        |> PassManager.queueTransform Passes.constantPropagationPass
        |> PassManager.queueTransform Passes.deadCodeEliminationPass
        |> PassManager.queueTransform Passes.instructionSimplifyPass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform Passes.jumpThreadingPass
        |> PassManager.queueTransform Passes.constantPropagationPass
        |> PassManager.queueTransform Passes.deadCodeEliminationPass
        |> PassManager.queueTransform Passes.instructionSimplifyPass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform (debugModulePasses "After optimizations:")
        |> PassManager.queueTransform Legalize.legalizeConstantsPass
        |> PassManager.queueTransform Legalize.breakPhiCriticalEdgesPass
        |> PassManager.queueTransform Passes.reorderBasicBlocksPass
        |> PassManager.queueTransform (debugModulePasses "End of IR:")
        |> PassManager.queueTransform GenAssembly.genAssemblyPass
        |> PassManager.queueTransform GenAssembly.removeUnusedLabelsPass
        |> PassManager.queueTransform GenAssembly.removeRedundantLabelsPass

    Lex.tokenize text
    |> Result.bind Parse.parse
    |> Result.bind Sema.analyse
    |> Result.map Codegen.codegen
    |> Result.map (fun x -> passManager |> PassManager.run x)

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
            eprintfn "%s" s
            -1
