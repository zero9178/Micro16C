module Tests

open System
open Micro16C.Backend.Assembly
open Xunit
open FsUnit.Xunit
open FsUnit.CustomMatchers

[<Fact>]
let ``For loop`` () =
    """
    int n = 0;
    for (int i = 0; i < 5; i++)
    {
        n += i;
    }
    R1 = n;
    """
    |> Micro16C.compile
    |> should be (ofCase <@ Result<seq<AssemblyLine>, string>.Ok @>)
