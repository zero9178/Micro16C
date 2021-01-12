module Tests

open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.IR
open Xunit
open FsUnit.Xunit

[<Fact>]
let IRReader () =

    let result =
        """; succ = ["%doWhileBody"] pred = []
        %entry:
                %0 = load R0
                %1 = load R1
                goto %doWhileBody

        ; succ = ["%modNeg"; "%cont"] pred = ["%boolCont.copy"; "%entry"]
        %doWhileBody:
                %3 = phi (%2,%boolCont.copy) (%1,%entry)
                %2 = phi (%4,%boolCont.copy) (%0,%entry)
                br %3 < 0 %modNeg %cont

        ; succ = ["%cont"] pred = ["%doWhileBody"]
        %modNeg:
                %5 = not %3
                %6 = add 1 %5
                goto %cont

        ; succ = ["%modBody"] pred = ["%modNeg"; "%doWhileBody"]
        %cont:
                %7 = phi (%6,%modNeg) (%3,%doWhileBody)
                %8 = not %2
                %9 = add 1 %8
                goto %modBody

        ; succ = ["%modEnd"; "%modCont"] pred = ["%modCont"; "%cont"]
        %modBody:
                %4 = phi (%10,%modCont) (%7,%cont)
                %10 = add %9 %4
                br %10 < 0 %modEnd %modCont

        ; succ = ["%modBody"] pred = ["%modBody"]
        %modCont:
                goto %modBody

        ; succ = ["%doWhileContinue"; "%boolCont.copy"] pred = ["%modBody"]
        %modEnd:
                br %4 = 0 %doWhileContinue %boolCont.copy

        ; succ = ["%doWhileBody"] pred = ["%modEnd"]
        %boolCont.copy:
                goto %doWhileBody

        ; succ = [] pred = ["%modEnd"]
        %doWhileContinue:
                store %2 -> R2"""
        |> IRReader.fromString
        |> (!)
        |> Module.asText

    result
    |> IRReader.fromString
    |> (!)
    |> Module.asText
    |> should equal result
