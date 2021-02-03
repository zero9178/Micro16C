module Tests


open Micro16C.Backend
open Micro16C.Backend.Assembly
open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.PassManager
open Micro16C.MiddleEnd.Tests.PassesTests
open Micro16C.Simulator
open Xunit
open FsUnit.Xunit

let private runIRWithState state irModule =
    PassManager.Default()
    |> PassManager.registerAnalysis Passes.analyzeDominancePass
    |> PassManager.registerAnalysis Passes.analyzeLivenessPass
    |> PassManager.registerAnalysis RegisterAllocator.allocateRegistersPass
    |> PassManager.queueTransform Legalize.legalizeConstantsPass
    |> PassManager.queueTransform Legalize.breakPhiCriticalEdgesPass
    |> PassManager.queueTransform GenAssembly.genAssemblyPass
    |> PassManager.queueTransform GenAssembly.genMachineCodePass
    |> PassManager.run irModule
    |> Simulator.simulateWithState state
    |> Seq.last

let private runIR irModule =
    PassManager.Default()
    |> PassManager.registerAnalysis Passes.analyzeDominancePass
    |> PassManager.registerAnalysis Passes.analyzeLivenessPass
    |> PassManager.registerAnalysis RegisterAllocator.allocateRegistersPass
    |> PassManager.queueTransform Legalize.legalizeConstantsPass
    |> PassManager.queueTransform Legalize.breakPhiCriticalEdgesPass
    |> PassManager.queueTransform GenAssembly.genAssemblyPass
    |> PassManager.queueTransform GenAssembly.genMachineCodePass
    |> PassManager.run irModule
    |> Simulator.simulate
    |> Seq.last

[<Fact>]
let ``Legalize Constants`` () =
    """
%entry:
    %0 = load R1
    %1 = add 1 %0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeConstantsPass
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
    |> testPass Legalize.legalizeConstantsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = shl 1 1
    %2 = shl %1 1
    %3 = add %2 1
    %4 = add %0 %3
    """)

    """
%entry:
    %0 = load R1
    %1 = add -5 %0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeConstantsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = add 1 1
    %2 = shl %1 1
    %3 = not %2
    %4 = add %0 %3
    """)

    """
%entry:
    %0 = load R1
    %1 = add -32768 %0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeConstantsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = lshr -1 1
    %2 = not %1
    %3 = add %0 %2
    """)

    """
%entry:
    %0 = load R1
    %1 = add 16383 %0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeConstantsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = lshr -1 1
    %2 = lshr %1 1
    %3 = add %0 %2
    """)

    """
%entry:
    %0 = load R1
    %1 = add 15 %0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeConstantsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = shl -1 1
    %2 = shl %1 1
    %3 = shl %2 1
    %4 = shl %3 1
    %5 = not %4
    %6 = add %0 %5
    """)

    """
%entry:
    %0 = load R1
    %1 = add 3 %0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeConstantsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = shl 1 1
    %2 = add 1 %1
    %3 = add %2 %0
    """)

    """
%entry:
    %0 = load R1
    %1 = add -4 %0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeConstantsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = shl -1 1
    %2 = shl %1 1
    %3 = add %2 %0
    """)

[<Fact>]
let ``Legalize instructions: Shifting`` () =
    """
%entry:
    %0 = load R0
    %1 = shl %0 5
    store %1 -> R1
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R0
    %1 = shl %0 1
    %2 = shl %1 1
    %3 = shl %2 1
    %4 = shl %3 1
    %5 = shl %4 1
    store %5 -> R1
    """)

    """
%entry:
    %0 = load R0
    %1 = lshr %0 5
    store %1 -> R1
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R0
    %1 = lshr %0 1
    %2 = lshr %1 1
    %3 = lshr %2 1
    %4 = lshr %3 1
    %5 = lshr %4 1
    store %5 -> R1
    """)

    """
%entry:
    %0 = load R0
    %1 = load R2
    %2 = shl %0 %1
    store %2 -> R2
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> runIRWithState
        { Simulator.State.Default with
              Registers =
                  [| 3s
                     0s
                     5s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s |] }
    |> (fun state -> state.Registers.[2])
    |> should equal (3s <<< 5)

[<Fact>]
let ``Legalize instructions: Negate`` () =
    """
%entry:
    %0 = load R0
    %1 = neg %0
    store %1 -> R0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> runIRWithState
        { Simulator.State.Default with
              Registers =
                  [| 3s
                     0s
                     5s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s |] }
    |> (fun state -> state.Registers.[0])
    |> should equal -3s

    """
%entry:
    %0 = load R0
    %1 = neg %0
    store %1 -> R0
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> runIRWithState
        { Simulator.State.Default with
              Registers =
                  [| -3s
                     0s
                     5s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s |] }
    |> (fun state -> state.Registers.[0])
    |> should equal 3s

[<Fact>]
let ``Legalize instructions: Multiply`` () =
    """
%entry:
    %0 = load R10
    %1 = load R9
    %2 = mul %0 %1
    store %2 -> R8
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> runIRWithState
        { Simulator.State.Default with
              Registers =
                  [| 0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     3s
                     5s |] }
    |> (fun state -> state.Registers.[8])
    |> should equal 15s

    """
%entry:
    %0 = load R10
    %1 = load R9
    %2 = mul %0 %1
    store %2 -> R8
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> runIRWithState
        { Simulator.State.Default with
              Registers =
                  [| 0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     -3s
                     5s |] }
    |> (fun state -> state.Registers.[8])
    |> should equal -15s

    """
%entry:
    %0 = load R10
    %1 = load R9
    %2 = mul %0 %1
    store %2 -> R8
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> runIRWithState
        { Simulator.State.Default with
              Registers =
                  [| 0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     -3s
                     -5s |] }
    |> (fun state -> state.Registers.[8])
    |> should equal 15s

    """
%entry:
    %0 = load R10
    %1 = load R9
    %2 = mul %0 %1
    store %2 -> R8
    """
    |> IRReader.fromString
    |> testPass Legalize.legalizeInstructionsPass
    |> runIRWithState
        { Simulator.State.Default with
              Registers =
                  [| 0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     0s
                     3s
                     -5s |] }
    |> (fun state -> state.Registers.[8])
    |> should equal -15s

let private runOnModule passManager (irModule: IR.Module ref) = passManager |> PassManager.run irModule

[<Fact>]
let ``Assembly folding`` () =
    let assembly =
        """
    %entry:
        %0 = load R0
        %1 = shl %0 1
        %2 = shl %1 1
        """
        |> IRReader.fromString
        |> (PassManager.Default()
            |> PassManager.registerAnalysis Passes.analyzeLivenessPass
            |> PassManager.registerAnalysis Passes.analyzeDominancePass
            |> PassManager.registerAnalysis RegisterAllocator.allocateRegistersPass
            |> PassManager.queueTransform GenAssembly.genAssemblyPass
            |> runOnModule)


    assembly |> should haveLength 2
    assembly.[0] |> should equal (Label "entry")

    (match assembly.[1] with
     | Operation { AMux = Some AMux.ABus
                   ABus = Some input1
                   BBus = Some input2
                   SBus = Some _
                   Shifter = Some Shifter.Left
                   ALU = Some ALU.Add } when input1 = input2 -> true
     | _ -> false)
    |> should be True

    let assembly =
        """
    %entry:
        %0 = load R0
        %1 = shl %0 1
        %2 = shl %1 1
        goto %false
    %true:
        store 0 -> R1

    %false:
        store 1 -> R2
        """
        |> IRReader.fromString
        |> (PassManager.Default()
            |> PassManager.registerAnalysis Passes.analyzeLivenessPass
            |> PassManager.registerAnalysis Passes.analyzeDominancePass
            |> PassManager.registerAnalysis RegisterAllocator.allocateRegistersPass
            |> PassManager.queueTransform GenAssembly.genAssemblyPass
            |> runOnModule)


    assembly
    |> List.length
    |> should be (greaterThanOrEqualTo 2)

    assembly.[0] |> should equal (Label "entry")

    (match assembly.[1] with
     | Operation { AMux = Some AMux.ABus
                   ABus = Some input1
                   BBus = Some input2
                   SBus = Some _
                   Shifter = Some Shifter.Left
                   ALU = Some ALU.Add
                   Condition = Some Cond.None
                   Address = Some "false" } when input1 = input2 -> true
     | _ -> false)
    |> should be True


    let assembly =
        """
    %entry:
        %0 = load 1
        %1 = add %1 1
        store %1 -> 1
        """
        |> IRReader.fromString
        |> (PassManager.Default()
            |> PassManager.registerAnalysis Passes.analyzeLivenessPass
            |> PassManager.registerAnalysis Passes.analyzeDominancePass
            |> PassManager.registerAnalysis RegisterAllocator.allocateRegistersPass
            |> PassManager.queueTransform GenAssembly.genAssemblyPass
            |> runOnModule)

    assembly
    |> List.exists (function
        | Operation { AMux = Some AMux.MBR
                      MemoryAccess = Some _ } -> true
        | _ -> false)
    |> should be False

[<Fact>]
let ``Assembly parsing`` () =
    let first =
        """
    :entry
        R0 <- lsh(1+1)
        R0 <- R0 + 1
        R1 <- lsh(1)
        R1 <- R1 + 1
        R2 <- R0 + R1; goto .entry
        """
        |> ParseAssembly.parseAssembly
        |> asText

    first
    |> ParseAssembly.parseAssembly
    |> asText
    |> should equal first

    let first =
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
        |> asText

    first
    |> ParseAssembly.parseAssembly
    |> asText
    |> should equal first

    let first =
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
        |> asText

    first
    |> ParseAssembly.parseAssembly
    |> asText
    |> should equal first

    let first =
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
        |> asText

    first
    |> ParseAssembly.parseAssembly
    |> asText
    |> should equal first

    let first =
        """
    ### Aufgabe 9 ###
MAR <- 0; rd
rd
R0 <- MBR
R1 <- 1
:loop
(R0); if Z goto .zero
(R0 & 1); if Z goto .cond
goto .found
:cond
R0 <- rsh(R0)
R1 <- R1 + 1
goto .loop
:found
MAR <- -1; MBR <- R1; wr
goto .end; wr
:zero
MAR <- -1; MBR <- 0; wr
goto .end; wr
:end
        """
        |> ParseAssembly.parseAssembly
        |> asText

    first
    |> ParseAssembly.parseAssembly
    |> asText
    |> should equal first

[<Fact>]
let ``Remove redundant labels`` () =

    """
        goto .label3
        :label1
        :label2
        :label3
        R0 <- 1
        """
    |> ParseAssembly.parseAssembly
    |> GenAssembly.removeRedundantLabels
    |> GenAssembly.genMachineCode
    |> List.ofSeq
    |> should
        matchList
           ("""
        goto .label1
        :label1
        R0 <- 1
        """
            |> ParseAssembly.parseAssembly
            |> GenAssembly.genMachineCode
            |> List.ofSeq)
