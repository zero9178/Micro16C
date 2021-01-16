module Tests


open Micro16C.Backend
open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.Tests.PassesTests
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Legalize Constants`` () =
    """
%entry:
    %0 = load R1
    %1 = add 1 %0
    """
    |> IRReader.fromString
    |> Legalize.legalizeConstants
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = add 1 %0
    """)

    """
%entry:
    %0 = load R1
    %1 = add 5 %0
    """
    |> IRReader.fromString
    |> Legalize.legalizeConstants
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = shl 1
    %2 = shl %1
    %3 = add %2 1
    %4 = add %0 %3
    """)

    """
%entry:
    %0 = load R1
    %1 = add -5 %0
    """
    |> IRReader.fromString
    |> Legalize.legalizeConstants
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = add 1 1
    %2 = shl %1
    %3 = not %2
    %4 = add %0 %3
    """)

    """
%entry:
    %0 = load R1
    %1 = add -32768 %0
    """
    |> IRReader.fromString
    |> Legalize.legalizeConstants
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = shr -1
    %2 = not %1
    %3 = add %0 %2
    """)
