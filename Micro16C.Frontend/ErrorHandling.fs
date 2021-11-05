module Micro16C.Frontend.ErrorHandling

let comb2 (successComb: 'a -> 'b -> 'c) (x: Result<'a, string>) (y: Result<'b, string>) =
    match (x, y) with
    | Ok x, Ok y -> successComb x y |> Ok
    | Error s, Ok _
    | Ok _, Error s -> Error s
    | Error s1, Error s2 -> s1 + s2 |> Error

let comb3 (successComb: 'a -> 'b -> 'c -> 'd) (x: Result<'a, string>) (y: Result<'b, string>) (z: Result<'c, string>) =
    match (x, y, z) with
    | Ok x, Ok y, Ok z -> successComb x y z |> Ok
    | Error s, Ok _, Ok _
    | Ok _, Error s, Ok _ -> Error s
    | Ok _, Ok _, Error s -> Error s
    | Error s1, Error s2, Ok _ -> s1 + s2 |> Error
    | Error s1, Ok _, Error s2 -> s1 + s2 |> Error
    | Ok _, Error s1, Error s2 -> s1 + s2 |> Error
    | Error s1, Error s2, Error s3 -> s1 + s2 + s3 |> Error

let pack2 x y = (x, y)

let comb4 (successComb: 'a -> 'b -> 'c -> 'd -> 'e)
          (x: Result<'a, string>)
          (y: Result<'b, string>)
          (z: Result<'c, string>)
          (w: Result<'d, string>)
          =
    let temp1 = comb2 pack2 x y
    let temp2 = comb2 pack2 z w
    comb2 (fun (x, y) (z, w) -> successComb x y z w) temp1 temp2

let prependResult x y = comb2 (fun x y -> x :: y) x y

let foldResults (seq: seq<Result<'a, string>>) =
    seq
    |> Seq.fold (fun x y -> prependResult y x) ([] |> Ok)
