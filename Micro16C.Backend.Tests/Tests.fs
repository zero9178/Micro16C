module Tests


open Micro16C.Backend
open Micro16C.Backend.Assembly
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
    |> Legalize.legalizeConstants
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
    |> Legalize.legalizeConstants
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = lshr -1 1
    %2 = not %1
    %3 = add %0 %2
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
    |> Legalize.legalizeInstructions
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
    |> Legalize.legalizeInstructions
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
        |> Passes.analyzeLiveness
        |> Passes.analyzeDominance
        |> RegisterAllocator.allocateRegisters
        |> GenAssembly.genAssembly


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
        |> Passes.analyzeLiveness
        |> Passes.analyzeDominance
        |> RegisterAllocator.allocateRegisters
        |> GenAssembly.genAssembly


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
        |> Passes.analyzeLiveness
        |> Passes.analyzeDominance
        |> RegisterAllocator.allocateRegisters
        |> GenAssembly.genAssembly

    assembly
    |> List.exists (function
        | Operation { AMux = Some AMux.MBR
                      MemoryAccess = Some _ } -> true
        | _ -> false)
    |> should be False

[<Fact>]
let ``Assembly parsing`` () =
    """
:entry
    R0 <- lsh(1+1)
    R0 <- R0 + 1
    R1 <- lsh(1)
    R1 <- R1 + 1
    R2 <- R0 + R1; goto .entry
    """
    |> ParseAssembly.parseAssembly
