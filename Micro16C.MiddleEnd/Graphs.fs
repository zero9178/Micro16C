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
