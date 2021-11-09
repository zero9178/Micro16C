module Micro16C.Web.Client.Main

open System
open Elmish
open Bolero
open Bolero.Html
open System.IO

open Micro16C.CompileCode
open Micro16C.Backend


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
      .CodeToCompile(model.codeToCompile, fun v -> dispatch (CompileCode v))
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