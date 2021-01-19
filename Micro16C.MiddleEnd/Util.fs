module Micro16C.MiddleEnd.Util

open System
open System.Collections.Generic
open System.Collections
open System.Collections.Immutable

[<RequireQualifiedAccess>]
module ImmutableMap =

    let ofList (list: ('Key * 'Value) list): ImmutableDictionary<'Key, 'Value> =
        ImmutableDictionary.CreateRange<'Key, 'Value>(HashIdentity.Reference, list |> List.map (KeyValuePair))

    let ofSeq (seq: seq<('Key * 'Value)>): ImmutableDictionary<'Key, 'Value> =
        ImmutableDictionary.CreateRange<'Key, 'Value>(HashIdentity.Reference, seq |> Seq.map (KeyValuePair))

    let empty<'Key, 'Value when 'Key: not struct> =
        ImmutableDictionary<'Key, 'Value>
            .Empty.WithComparers(HashIdentity.Reference)

    let tryFind value (map: ImmutableDictionary<_, _>) =
        match map.TryGetValue value with
        | (false, _) -> None
        | (true, value) -> Some value

    let find value (map: ImmutableDictionary<_, _>) = tryFind value map |> Option.get

    let inline add key value (map: ImmutableDictionary<_, _>) = map.SetItem(key, value)

    let inline map f (map: ImmutableDictionary<_, _>) =
        map
        |> Seq.map (fun kv -> kv.Deconstruct() ||> f)
        |> ofSeq

    let iter f =
        Seq.iter (fun (kv: KeyValuePair<_, _>) -> kv.Deconstruct() ||> f)

    let isEmpty (map: ImmutableDictionary<_, _>) = map.IsEmpty

    let filter p =
        Seq.filter (fun (kv: KeyValuePair<_, _>) -> kv.Deconstruct() ||> p)
        >> (fun x -> ImmutableDictionary.CreateRange(HashIdentity.Reference, x))

    let remove key (map: ImmutableDictionary<_, _>) = map.Remove key

    let contains key (map: ImmutableDictionary<_, _>) = map.ContainsKey key

type ImmutableSet<'Key when 'Key: equality and 'Key: not struct>(set: ImmutableHashSet<'Key>) =

    member private this.HashSet = set.WithComparer(HashIdentity.Reference)

    new(seq: seq<'Key>) = ImmutableSet(ImmutableHashSet.CreateRange(HashIdentity.Reference, seq))

    member this.Add value = this.HashSet.Add value |> ImmutableSet

    member this.Contains item = this.HashSet.Contains item
    member this.IsProperSubsetOf enumerable = this.HashSet.IsProperSubsetOf enumerable

    member this.IsProperSupersetOf enumerable =
        this.HashSet.IsProperSupersetOf enumerable

    member this.IsSubsetOf enumerable = this.HashSet.IsSubsetOf enumerable
    member this.IsSupersetOf enumerable = this.HashSet.IsSupersetOf enumerable
    member this.Overlaps enumerable = this.HashSet.Overlaps enumerable
    member this.SetEquals enumerable = this.HashSet.SetEquals enumerable

    member this.GetEnumerator() = this.HashSet.GetEnumerator()

    member this.Count = this.HashSet.Count

    interface IEnumerable<'Key> with
        member this.GetEnumerator() =
            this.GetEnumerator() :> IEnumerator<'Key>

        member this.GetEnumerator() = this.GetEnumerator() :> IEnumerator

    interface IReadOnlyCollection<'Key> with
        member this.Count = this.Count

    interface IReadOnlySet<'Key> with
        member this.Contains item = this.Contains item
        member this.IsProperSubsetOf enumerable = this.IsProperSubsetOf enumerable

        member this.IsProperSupersetOf enumerable = this.IsProperSupersetOf enumerable

        member this.IsSubsetOf enumerable = this.IsSubsetOf enumerable
        member this.IsSupersetOf enumerable = this.IsSupersetOf enumerable
        member this.Overlaps enumerable = this.Overlaps enumerable
        member this.SetEquals enumerable = this.SetEquals enumerable

    interface IEquatable<ImmutableSet<'Key>> with
        member this.Equals other =
            (this :> IReadOnlySet<'Key>).SetEquals other

    override this.Equals other =
        (this :> IEquatable<ImmutableSet<'Key>>)
            .Equals(other :?> ImmutableSet<'Key>)

    override this.GetHashCode(): int =
        this.HashSet
        |> Seq.map (fun (x: 'Key) -> x.GetHashCode())
        |> Seq.reduce ((^^^))

    member this.Except other =
        this.HashSet.Except other |> ImmutableSet

    member this.Intersect other =
        this.HashSet.Intersect other |> ImmutableSet

    member this.Union other = this.HashSet.Union other |> ImmutableSet

    member this.Remove value =
        this.HashSet.Remove value |> ImmutableSet

    member this.IsEmpty = this.HashSet.IsEmpty

[<RequireQualifiedAccess>]
module ImmutableSet =

    let ofList (list: 'Key list) = ImmutableSet(list)

    let ofSeq (seq: seq<'Key>) = ImmutableSet(seq)

    let empty<'Key when 'Key: not struct and 'Key: equality> = ImmutableSet<'Key>(Seq.empty)

    let add value (set: ImmutableSet<'Key>) = set.Add(value)

    let contains value (set: ImmutableSet<'Key>) = set.Contains(value)

    let count (set: ImmutableSet<'Key>) = set.Count

    let difference (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.Except set2

    let exists predicate (set: ImmutableSet<'Key>) = set |> Seq.exists predicate

    let filter predicate (set: ImmutableSet<'Key>) = set |> Seq.filter predicate |> ofSeq

    let fold folder state (set: ImmutableSet<'Key>) = set |> Seq.fold folder state |> ofSeq

    let foldBack folder (set: ImmutableSet<'Key>) state = Seq.foldBack folder set state

    let forall predicate (set: ImmutableSet<'Key>) = set |> Seq.forall predicate

    let intersect (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.Intersect set2

    let intersectMany (sets: seq<ImmutableSet<'Key>>) =
        if sets |> Seq.isEmpty then empty else sets |> Seq.reduce intersect

    let isEmpty (set: ImmutableSet<'Key>) = set.IsEmpty

    let isProperSubset (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.IsProperSubsetOf set2

    let isProperSuperset (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.IsProperSupersetOf set2

    let isSubset (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.IsSubsetOf set2

    let isSuperset (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.IsSupersetOf set2

    let iter action (set: ImmutableSet<'Key>) = set |> Seq.iter action

    let map mapping (set: ImmutableSet<'Key>) = set |> Seq.map mapping |> ofSeq

    let remove value (set: ImmutableSet<'Key>) = set.Remove value

    let toList (set: ImmutableSet<'Key>) = set |> List.ofSeq

    let toSeq (set: ImmutableSet<'Key>) = set :> seq<'Key>

    let union (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.Union set2

    let unionMany (sets: seq<ImmutableSet<'Key>>) =
        if sets |> Seq.isEmpty then empty else sets |> Seq.reduce union

    let equal (set1: ImmutableSet<'Key>) (set2: ImmutableSet<'Key>) = set1.SetEquals set2

let associateValue v x = (x, v)

let associateWith f x = (x, f x)
