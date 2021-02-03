module Tests

open Micro16C.Backend
open Micro16C.Simulator
open Xunit
open FsUnit.Xunit
open FsUnit.CustomMatchers

[<Fact>]
let ``For loop`` () =
    let result =
        """
        int n = 0;
        for (int i = 0; i < 5; i++)
        {
            n += i;
        }
        R1 = n;
        """
        |> Micro16C.compile
        |> Result.map
            (GenAssembly.genMachineCode ()
             >> Simulator.simulate
             >> Seq.last)

    match result with
    | Error _ -> Assert.True(false)
    | Ok state -> state.Registers.[1] |> should equal 10s

[<Fact>]
let ``Break and continue`` () =

    let result =
        """
        int n = 0,i = 0;
        for (;;)
        {
            if (i == 5)
            {
                break;
            }
            if (i % 2 == 0)
            {
                n += i;
            }
            i++;
        }
        R1 = n;
        """
        |> Micro16C.compile
        |> Result.map
            (GenAssembly.genMachineCode ()
             >> Simulator.simulate
             >> Seq.last)

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok state -> state.Registers.[1] |> should equal 6s

    let result =
        """
        int n = 0,i = 0;
        for (;;)
        {
            if (i == 5)
            {
                break;
            }
            n += i++;
        }
        R1 = n;
        """
        |> Micro16C.compile
        |> Result.map
            (GenAssembly.genMachineCode ()
             >> Simulator.simulate
             >> Seq.last)

    match result with
    | Error msg -> Assert.True(false, msg)
    | Ok state -> state.Registers.[1] |> should equal 10s
