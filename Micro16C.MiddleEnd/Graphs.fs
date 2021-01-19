[<RequireQualifiedAccess>]
module Micro16C.MiddleEnd.Graphs

open Micro16C.MiddleEnd.Util

let preOrder (successorsFunc: 'Node -> seq<'Node>) (root: 'Node) =
    let mutable set = ImmutableSet.empty

    let rec seqImpl current =
        if set |> ImmutableSet.contains current then
            Seq.empty
        else
            set <- set |> ImmutableSet.add current

            seq {
                yield current

                yield!
                    current
                    |> successorsFunc
                    |> Seq.map seqImpl
                    |> Seq.concat
            }

    root |> seqImpl

let postOrder (successorsFunc: 'Node -> seq<'Node>) (root: 'Node) =
    let mutable set = ImmutableSet.empty

    let rec seqImpl current =
        if set |> ImmutableSet.contains current then
            Seq.empty
        else
            set <- set |> ImmutableSet.add current

            seq {
                yield!
                    current
                    |> successorsFunc
                    |> Seq.map seqImpl
                    |> Seq.concat

                yield current
            }

    root |> seqImpl

let reversePostOrder successorsFunc root =
    postOrder successorsFunc root
    |> Seq.rev
    |> Seq.cache

let singleForwardAnalysis transform join predecessorsFun successorsFunc root =
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
