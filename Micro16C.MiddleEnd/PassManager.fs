module Micro16C.MiddleEnd.PassManager

open Micro16C.MiddleEnd.Util

type IAnalysis =
    abstract DependsOn : IAnalysis list

type IPassManager =
    abstract AnalysisData : ImmutableMap<IAnalysis, obj>

[<ReferenceEquality>]
type Analysis<'In, 'Out> =
    { Pass: IPassManager -> 'In -> 'Out
      DependsOn: IAnalysis list }
    interface IAnalysis with
        member this.DependsOn = this.DependsOn

type ITransformation =
    abstract DependsOn : IAnalysis list
    abstract Invalidates : IAnalysis list

and [<NoEquality; NoComparison>] Transformation<'In, 'Out> =
    { Pass: IPassManager -> 'In -> 'Out
      DependsOn: IAnalysis list
      Invalidates: IAnalysis list }
    interface ITransformation with
        member this.DependsOn = this.DependsOn
        member this.Invalidates = this.Invalidates

and [<NoEquality; NoComparison>] PassManager<'In, 'Out> =
    private
        { TransformPasses: (ITransformation * (ITransformation -> IPassManager -> obj -> obj)) list
          AnalysisPasses: ImmutableMap<IAnalysis, IAnalysis -> IPassManager -> obj -> obj>
          AnalysisData: ImmutableMap<IAnalysis, obj> }
    interface IPassManager with
        member this.AnalysisData = this.AnalysisData

module PassManager =

    let Default () : PassManager<'In, 'Out> =
        { TransformPasses = []
          AnalysisPasses = ImmutableMap.empty
          AnalysisData = ImmutableMap.empty }

    let queueTransform
        (pass: Transformation<'In, 'Out>)
        (passManager: PassManager<'Start, 'In>)
        : PassManager<'Start, 'Out> =
        { TransformPasses =
              (pass :> ITransformation,
               (fun pass manager input -> ((pass :?> Transformation<'In, 'Out>).Pass manager (input :?> 'In)) :> obj))
              :: passManager.TransformPasses
          AnalysisPasses = passManager.AnalysisPasses
          AnalysisData = passManager.AnalysisData }

    let registerAnalysis (pass: Analysis<'In, 'Out>) passManager =
        { passManager with
              AnalysisPasses =
                  passManager.AnalysisPasses
                  |> ImmutableMap.add
                      (pass :> IAnalysis)
                      (fun pass manager input -> ((pass :?> Analysis<'In, 'Out>).Pass manager (input :?> 'In)) :> obj) }

    let tryAnalysisData (analysis: Analysis<_, 'T>) (passManager: IPassManager) =
        passManager.AnalysisData
        |> ImmutableMap.tryFind (analysis :> IAnalysis)
        |> Option.map (fun x -> x :?> 'T)

    let analysisData analysis passManager =
        tryAnalysisData analysis passManager |> Option.get

    let run (input: 'In) (passManager: PassManager<'In, 'Out>) : 'Out =

        let rec ensureAnalysis analysis input passManager =
            if passManager.AnalysisData
               |> ImmutableMap.contains analysis then
                passManager
            else
                let passManager =
                    analysis.DependsOn
                    |> List.fold (fun passManager analysis -> ensureAnalysis analysis input passManager) passManager

                { passManager with
                      AnalysisData =
                          passManager.AnalysisData
                          |> ImmutableMap.add
                              analysis
                              (passManager.AnalysisPasses.[analysis] analysis (passManager :> IPassManager) input) }

        passManager.TransformPasses
        |> List.rev
        |> List.fold
            (fun (data, passManager) (pass, apply) ->
                let passManager =
                    pass.DependsOn
                    |> List.fold (fun passManager analysis -> ensureAnalysis analysis data passManager) passManager

                let data =
                    apply pass (passManager :> IPassManager) data

                let passManager =
                    pass.Invalidates
                    |> List.fold
                        (fun passManager analysis ->
                            { passManager with
                                  AnalysisData =
                                      passManager.AnalysisData
                                      |> ImmutableMap.remove analysis })
                        passManager

                (data, passManager))
            (input :> obj, passManager)
        |> fst
        :?> 'Out
