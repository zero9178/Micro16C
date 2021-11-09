module Micro16C.Web.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open System.IO
open Micro16C.Backend
open Micro16C.Frontend
open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.PassManager

let private debugModulePasses title =
    { Pass =
          (fun _ irModule ->
#if DEBUG
              printf $"%s{title}\n%s{(!irModule) |> IR.Module.asText}\n"
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
        |> PassManager.queueTransform Passes.deadBitEliminationPass
        |> PassManager.queueTransform Passes.jumpThreadingPass
        |> PassManager.queueTransform Passes.constantPropagationPass
        |> PassManager.queueTransform Passes.deadCodeEliminationPass
        |> PassManager.queueTransform Passes.instructionSimplifyPass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform Legalize.legalizeInstructionsPass
        |> PassManager.queueTransform Passes.constantPropagationPass
        |> PassManager.queueTransform Passes.deadCodeEliminationPass
        |> PassManager.queueTransform Passes.instructionSimplifyPass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform Passes.deadBitEliminationPass
        |> PassManager.queueTransform Passes.jumpThreadingPass
        |> PassManager.queueTransform Passes.constantPropagationPass
        |> PassManager.queueTransform Passes.deadCodeEliminationPass
        |> PassManager.queueTransform Passes.instructionSimplifyPass
        |> PassManager.queueTransform Passes.simplifyCFGPass
        |> PassManager.queueTransform Passes.deadBitEliminationPass
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


/// The Elmish application's model.
type Model =
    {
        counter: int
        error: string option
        codeToCompile: string
        generatedCode: string
    }


let initModel =
    {
        counter = 0
        error = None
        codeToCompile = "int r0 = R0;
int r1 = R1;
int mod;
do
{
    mod = r1 % r0;
    r1 = r0;
    r0 = mod;
}
while(mod != 0);
R2 = r1;"
        generatedCode = ""
    }


/// The Elmish application's update messages.
type Message =
    | Increment
    | Decrement
    | SetCounter of int
    | Error of exn
    | ClearError
    | CompileCode of string

let update message model =
    match message with

    | Increment ->
        { model with counter = model.counter + 1 }
    | Decrement ->
        { model with counter = model.counter - 1 }
    | SetCounter value ->
        { model with counter = value }

    | Error exn ->
        { model with error = Some exn.Message }
    | ClearError ->
        { model with error = None }
    | CompileCode value ->
        { model with codeToCompile = value; generatedCode = match compile value |> Result.map Assembly.asText with 
                                                            | Ok value -> value
                                                            | Result.Error s -> "Error compiling: " + s }

type Main = Template<"wwwroot/main.html">

let homePage model dispatch =
    Main.Home()
      .CodeToCompile(model.codeToCompile, CompileCode >> dispatch)
      .GeneratedCode(model.generatedCode)
      .Elt()

let view model dispatch =
    Main()
        .Body(homePage model dispatch)
        .Error(
            cond model.error <| function
            | None -> empty
            | Some err ->
                Main.ErrorNotification()
                    .Text(err)
                    .Hide(fun _ -> dispatch ClearError)
                    .Elt()
        )
        .Elt()

let program = Program.mkSimple (fun _ -> initModel) update view

type MyApp() =
    inherit ProgramComponent<Model, Message>()

    override this.Program = program