module Micro16C.MiddleEnd.Util

open System.Collections.Generic
open System.Collections.Immutable

module ImmutableMap =
    let tryFind value (map: IImmutableDictionary<'Key, 'Value>) =
        match map.TryGetValue value with
        | (false, _) -> None
        | (true, value) -> Some value

    let inline add key value (map: ^T): ^T :> IImmutableDictionary<'Key, 'Value> = map.SetItem(key, value) :?> ^T

    let inline map f (map: ^T): ^T :> IImmutableDictionary<'Key, 'Value> =
        map
        |> Seq.map (fun kv ->
            let key, value = kv.Deconstruct()
            let key, value = f (key, value)
            KeyValuePair(key, value))
        |> map.SetItems :?> ^T

    let iter f =
        Seq.iter (fun (kv: KeyValuePair<'U, 'V>) -> kv.Deconstruct() |> f)
