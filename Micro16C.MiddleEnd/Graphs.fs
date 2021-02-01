[<RequireQualifiedAccess>]
module Micro16C.MiddleEnd.Graphs

open Micro16C.MiddleEnd.Util

let preOrder (successorsFunc: 'Node -> seq<'Node>) (root: 'Node) =

    Seq.unfold (fun (list, set) ->
        match list with
        | [] -> None
        | head :: rest when set |> ImmutableSet.contains head -> Some(None, (rest, set))
        | head :: rest ->
            let set = set |> ImmutableSet.add head
            Some(Some head, ((head |> successorsFunc |> List.ofSeq) @ rest, set))) ([ root ], ImmutableSet.empty)
    |> Seq.choose id

let postOrder (successorsFunc: 'Node -> seq<'Node>) (root: 'Node) =

    Seq.unfold (fun (list, set, outputted) ->
        match list with
        | [] -> None
        | head :: rest when set |> ImmutableSet.contains head ->
            let successors = head |> successorsFunc |> List.ofSeq

            if outputted |> ImmutableSet.contains head |> not
               && successors
                  |> List.forall (fun x -> set |> ImmutableSet.contains x) then
                let outputted = outputted |> ImmutableSet.add head
                Some(Some head, (rest, set, outputted))
            else
                Some(None, (rest, set, outputted))
        | head :: _ ->
            let successors =
                head
                |> successorsFunc
                |> Seq.filter (fun x -> set |> ImmutableSet.contains x |> not)
                |> List.ofSeq

            let set = set |> ImmutableSet.add head
            Some(None, (successors @ list, set, outputted))) ([ root ], ImmutableSet.empty, ImmutableSet.empty)
    |> Seq.choose id

let reversePostOrder successorsFunc root = postOrder successorsFunc root |> Seq.rev

let dataFlowAnalysis transform join predecessorsFun successorsFunc root =

    Seq.unfold (fun (workList, outs) ->
        match workList |> Seq.tryHead with
        | None -> None
        | Some head ->

            let workList = workList |> ImmutableSet.remove head

            let inValue =
                head
                |> predecessorsFun
                |> Seq.map (fun x -> (x, outs |> ImmutableMap.tryFind x))
                |> join head

            let out = transform inValue head

            let outs, changes =
                (match outs |> ImmutableMap.tryFind head with
                 | None -> (outs |> ImmutableMap.add head out, true)
                 | Some before when before <> out -> (outs |> ImmutableMap.add head out, true)
                 | _ -> (outs, false))

            if changes then
                let workList =
                    workList
                    |> ImmutableSet.union (head |> successorsFunc |> ImmutableSet.ofSeq)

                Some(outs, (workList, outs))
            else
                Some(outs, (workList, outs))) (ImmutableSet.ofList [ root ], ImmutableMap.empty)
    |> Seq.last
