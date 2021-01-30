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

let forwardAnalysis transform join predecessorsFun successorsFunc root =
    let mutable outs = ImmutableMap.empty

    let seq =
        reversePostOrder successorsFunc root |> Seq.cache

    Seq.initInfinite (fun _ ->
        Seq.fold (fun changes b ->

            let inValue =
                b
                |> predecessorsFun
                |> Seq.map (fun x -> (x, outs |> ImmutableMap.tryFind x))
                |> join b

            let out = transform inValue b

            let changes =
                (match outs |> ImmutableMap.tryFind b with
                 | None ->
                     outs <- outs |> ImmutableMap.add b out
                     true
                 | Some before when before <> out ->
                     outs <- outs |> ImmutableMap.add b out
                     true
                 | _ -> false)
                || changes

            changes) false seq)
    |> Seq.takeWhile id
    |> Seq.tryLast
    |> ignore

let backwardAnalysis transform join successorsFunc root =
    let mutable outs = ImmutableMap.empty

    let seq =
        postOrder successorsFunc root |> Seq.cache

    Seq.initInfinite (fun _ ->
        Seq.fold (fun changes b ->

            let inValue =
                b
                |> successorsFunc
                |> Seq.map (fun x -> (x, outs |> ImmutableMap.tryFind x))
                |> join b

            let out = transform inValue b

            let changes =
                (match outs |> ImmutableMap.tryFind b with
                 | None ->
                     outs <- outs |> ImmutableMap.add b out
                     true
                 | Some before when before <> out ->
                     outs <- outs |> ImmutableMap.add b out
                     true
                 | _ -> false)
                || changes

            changes) false seq)
    |> Seq.takeWhile id
    |> Seq.tryLast
    |> ignore
