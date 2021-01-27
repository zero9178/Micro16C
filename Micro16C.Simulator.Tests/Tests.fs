module Tests

open System
open Micro16C.Backend
open Micro16C.Simulator
open Xunit
open FsUnit.Xunit

[<Fact>]
let ``Simulator Tests`` () =
    let endState =
        """
    ### Aufgabe 6 ###
R0 <- lsh(1)
R0 <- lsh(R0 + 1)
R0 <- R0 + 1
R1 <- lsh(1)
R1 <- lsh(R1)
MAR <- R1; rd
rd
R1 <- MBR
R2 <- 0
R3 <- rsh(-1)
R3 <- ~R3
:maskLoop
(R0); if Z goto .doneMask
R2 <- rsh(R2)
R2 <- R2 + R3
R0 <- R0 + (-1); goto .maskLoop
:doneMask
R3 <- R1 & R2
R3 <- ~R3
R1 <- R3 & R1
R2 <- R3 & R2
R1 <- ~R1
R2 <- ~R2
R3 <- R1 & R2
MBR <- ~R3; wr
wr
    """
        |> ParseAssembly.parseAssembly
        |> GenAssembly.genMachineCode
        |> Simulator.simulate
        |> Seq.last

    endState.Registers.[1] |> should equal -1s

    endState.Registers.[2]
    |> should equal 0b111111111s

    endState.Registers.[3]
    |> should equal 0b111111111s

    endState.MAR |> should equal 4s

    endState.Memory.[4]
    |> should equal (~~~0b111111111s)

    let endState =
        """
    ### Aufgabe 7 ###
R0 <- lsh(1)
R0 <- lsh(R0+1)
R0 <- lsh(R0+1)
R0 <- R0 + 1
R1 <- lsh(1)
R1 <- lsh(R1 + 1)
R1 <- lsh(R1)
:euclid
(R1); if Z goto .end
R3 <- R0
:mod
R4 <- ~R1
R4 <- R4 + 1
(R3 + R4); if N goto .endMod
R3 <- R3 + R4; goto .mod
:endMod
R0 <- R1
R1 <- R3; goto .euclid
:end
R2 <- R1
    """
        |> ParseAssembly.parseAssembly
        |> GenAssembly.genMachineCode
        |> Simulator.simulate
        |> Seq.last

    endState.Registers.[0] |> should equal 3s

    let endState =
        """
    ### Aufgabe 8 ###
R0 <- lsh(1)
R0 <- lsh(R0 + 1)
R0 <- R0 + 1
R1 <- lsh(1)
R1 <- lsh(R1)
R1 <- lsh(R1)
R1 <- lsh(R1)
R1 <- R1 + 1
:loop
(R0); if N goto .end
R2 <- ~R0; MAR <- R0; rd
R2 <- R2 + 1; rd
R3 <- MBR
R2 <- R1 + R2
MAR <- R2; rd
rd
R2 <- MBR
MAR <- R2; rd
rd
MAR <- R0; wr
wr
MBR <- R3; MAR <- R2; wr
wr
R0 <- R0 + (-1)
goto .loop
:end
    """
        |> ParseAssembly.parseAssembly
        |> GenAssembly.genMachineCode
        |> Simulator.simulate
        |> Seq.last

    endState.Registers.[0] |> should equal -1s
    endState.Registers.[1] |> should equal 17s
