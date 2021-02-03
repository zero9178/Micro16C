module Micro16C.MiddleEnd.PassManager

open Micro16C.MiddleEnd.Util

type IAnalysis =
    abstract DependsOn: IAnalysis list


[<ReferenceEquality>]
type Analysis<'In, 'Out> =
    { Pass: 'In -> 'Out
      DependsOn: IAnalysis list }
    interface IAnalysis with
        member this.DependsOn = this.DependsOn

type ITransformation =
    abstract DependsOn: IAnalysis list
    abstract Invalidates: IAnalysis list

and [<NoEquality; NoComparison>] Transformation<'In, 'Out> =
    { Pass: 'In -> 'Out
      DependsOn: IAnalysis list
      Invalidates: IAnalysis list }
    interface ITransformation with
        member this.DependsOn = this.DependsOn
        member this.Invalidates = this.Invalidates

and [<NoEquality; NoComparison>] PassManager =
    private
        { TransformPasses: (ITransformation * (ITransformation -> obj -> obj)) list
          AnalysisPasses: ImmutableMap<IAnalysis, IAnalysis -> obj -> obj>
          AnalysisData: ImmutableMap<IAnalysis, obj> }

        static member Default =
            { TransformPasses = []
              AnalysisPasses = ImmutableMap.empty
              AnalysisData = ImmutableMap.empty }

module PassManager =

    let queueTransform (pass: Transformation<'In, 'Out>) passManager =
        { passManager with
              TransformPasses =
                  (pass :> ITransformation,
                   (fun pass input ->
                       ((pass :?> Transformation<'In, 'Out>)
                           .Pass(input :?> 'In)) :> obj))
                  :: passManager.TransformPasses }

    let registerAnalysis (pass: Analysis<'In, 'Out>) passManager =
        { passManager with
              AnalysisPasses =
                  passManager.AnalysisPasses
                  |> ImmutableMap.add (pass :> IAnalysis) (fun pass input ->
                         ((pass :?> Analysis<'In, 'Out>).Pass(input :?> 'In)) :> obj) }

    let tryAnalysisData (analysis: Analysis<_, 'T>) passManager =
        passManager.AnalysisData
        |> ImmutableMap.tryFind (analysis :> IAnalysis)
        |> Option.map (fun x -> x :?> ^T)

    let analysisData analysis passManager =
        tryAnalysisData analysis passManager |> Option.get

    let run irModule passManager =

        let rec ensureAnalysis analysis irModule passManager =
            if passManager.AnalysisData
               |> ImmutableMap.contains analysis then
                passManager
            else
                let passManager =
                    analysis.DependsOn
                    |> List.fold (fun passManager analysis -> ensureAnalysis analysis irModule passManager) passManager

                { passManager with
                      AnalysisData =
                          passManager.AnalysisData
                          |> ImmutableMap.add analysis (passManager.AnalysisPasses.[analysis] analysis irModule) }

        passManager.TransformPasses
        |> List.rev
        |> List.fold (fun (data, passManager) (pass, apply) ->
            let passManager =
                pass.DependsOn
                |> List.fold (fun passManager analysis -> ensureAnalysis analysis data passManager) passManager

            let data = apply pass data

            let passManager =
                pass.Invalidates
                |> List.fold (fun passManager analysis ->
                    { passManager with
                          AnalysisData =
                              passManager.AnalysisData
                              |> ImmutableMap.remove analysis }) passManager

            (data, passManager)) (irModule :> obj, passManager)
