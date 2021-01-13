module Micro16C.MiddleEnd.Tests.PassesTests

open System
open FsUnit
open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util
open NHamcrest.Core
open Xunit

let private structurallyEquivalentToImpl string irModule =

    let isCommutative =
        function
        | Ref { Content = BinaryInstruction _ }
        | Ref { Content = PhiInstruction _ } -> true
        | _ -> false

    let expectedModule = string |> IRReader.fromString

    let sameInstrKind actualInstr expectedInstr =
        match (!actualInstr, !expectedInstr) with
        | { Content = BinaryInstruction { Kind = k1 } }, { Content = BinaryInstruction { Kind = k2 } } when k1 = k2 ->
            true
        | { Content = UnaryInstruction { Kind = k1 } }, { Content = UnaryInstruction { Kind = k2 } } when k1 = k2 ->
            true
        | { Content = CondBrInstruction { Kind = k1 } }, { Content = CondBrInstruction { Kind = k2 } } when k1 = k2 ->
            true
        | { Content = PhiInstruction { Incoming = l1 } }, { Content = PhiInstruction { Incoming = l2 } } when List.length
                                                                                                                  l1 = List.length
                                                                                                                           l2 ->
            true
        | { Content = AllocationInstruction _ }, { Content = AllocationInstruction _ } -> true
        | { Content = LoadInstruction _ }, { Content = LoadInstruction _ } -> true
        | { Content = StoreInstruction _ }, { Content = StoreInstruction _ } -> true
        | { Content = GotoInstruction _ }, { Content = GotoInstruction _ } -> true
        | { Content = CopyInstruction _ }, { Content = CopyInstruction _ } -> true
        | { Content = BasicBlockValue _ }, { Content = BasicBlockValue _ } -> true
        | _ -> false

    let operandEqual (operandEqual, actualToExpected) actualOperand expectedOperand =
        match operandEqual, !actualOperand, !expectedOperand with
        | false, _, _ -> (false, actualToExpected)
        | _, { Content = Constant { Value = c1 } }, { Content = Constant { Value = c2 } } -> (c1 = c2, actualToExpected)
        | _, { Content = Undef }, { Content = Undef } -> (true, actualToExpected)
        | _, { Content = Register r1 }, { Content = Register r2 } -> (r1 = r2, actualToExpected)
        | _, { Content = BasicBlockValue _ }, { Content = BasicBlockValue _ }
        | _, _, _ when sameInstrKind actualOperand expectedOperand ->
            match actualToExpected
                  |> ImmutableMap.tryFind actualOperand with
            | Some expected -> (expected = expectedOperand, actualToExpected)
            | None ->
                (true,
                 actualToExpected
                 |> ImmutableMap.add actualOperand expectedOperand)
        | _, _, _ -> (false, actualToExpected)

    let rec distribute e =
        function
        | [] -> [ [ e ] ]
        | x :: xs' as xs ->
            (e :: xs)
            :: [ for xs in distribute e xs' -> x :: xs ]

    let rec permute =
        function
        | [] -> [ [] ]
        | e :: xs -> List.collect (distribute e) (permute xs)

    List.fold2 (fun (equivalent, actualToExpected) bbActual bbExpect ->
        if not equivalent then
            (false, actualToExpected)
        else
            match actualToExpected |> ImmutableMap.tryFind bbActual with
            | Some expected -> (bbExpect = expected, actualToExpected)
            | None ->
                let actualToExpected =
                    ImmutableMap.add bbActual bbExpect actualToExpected

                List.fold2 (fun (equivalent, actualToExpected) actualInstr expectedInstr ->
                    if not equivalent then
                        (false, actualToExpected)
                    else
                        let equivalent = sameInstrKind actualInstr expectedInstr

                        if not equivalent then
                            (false, actualToExpected)
                        else
                            let equivalent, actualToExpected =
                                if isCommutative actualInstr then
                                    let actualOperands = !actualInstr |> Value.operands
                                    let expectedOperands = !expectedInstr |> Value.operands

                                    let identityPermutation =
                                        List.init (List.length actualOperands) id

                                    let allPermutations =
                                        permute identityPermutation
                                        |> List.map (Array.ofList)

                                    allPermutations
                                    |> Seq.map (fun permutation ->
                                        let actualOperands =
                                            actualOperands
                                            |> List.permute (Array.get permutation)

                                        List.fold2
                                            operandEqual
                                            (true, actualToExpected)
                                            (actualOperands)
                                            (expectedOperands))
                                    |> Seq.tryFind fst
                                    |> Option.defaultValue (false, actualToExpected)
                                else
                                    List.fold2
                                        operandEqual
                                        (true, actualToExpected)
                                        (!actualInstr |> Value.operands)
                                        (!expectedInstr |> Value.operands)

                            if equivalent then

                                if Value.producesValue !actualInstr then
                                    match actualToExpected
                                          |> ImmutableMap.tryFind actualInstr with
                                    | Some expected -> (expected = expectedInstr, actualToExpected)
                                    | None ->
                                        (true,
                                         actualToExpected
                                         |> ImmutableMap.add actualInstr expectedInstr)
                                else
                                    (true, actualToExpected)
                            else
                                (false, actualToExpected)) (equivalent, actualToExpected)
                    (!bbActual
                     |> Value.asBasicBlock
                     |> BasicBlock.instructions)
                    (!bbExpect
                     |> Value.asBasicBlock
                     |> BasicBlock.instructions)) (true, ImmutableMap.empty) (!irModule |> Module.basicBlocks)
        (!expectedModule |> Module.basicBlocks)
    |> fst

let structurallyEquivalentTo source =
    CustomMatcher<obj>
        (source,
         Func<obj, bool>(fun x ->
             try
                 x :?> Module ref
                 |> structurallyEquivalentToImpl source
             with _ -> false))

[<Fact>]
let ``Instruction Simplify: And patterns`` () =
    """%entry:
    %0 = load R0
    %1 = and 0 %0
    store %1 -> R1
    """
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
    %entry:
        %0 = load R0
        store 0 -> R1
    """)

    """%entry:
        %0 = load R0
        %1 = and %0 0
        store %1 -> R1
    """
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
    %entry:
        %0 = load R0
        store 0 -> R1
    """)

    """%entry:
        %0 = load R0
        %1 = and %0 -1
        store %1 -> R1
    """
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
    %entry:
        %0 = load R0
        store %0 -> R1
    """)

    """%entry:
        %1 = and 17 5
        store %1 -> R1
    """
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
    %entry:
        store 1 -> R1
    """)

    """%entry:
        %0 = load R0
        %1 = and %0 %0
        store %1 -> R1
    """
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
    %entry:
        %0 = load R0
        store %0 -> R1
    """)

[<Fact>]
let ``Instruction Simplify: Add patterns`` () =
    """%entry:
    %0 = load R0
    %1 = add %0 0
    store %1 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R0
    store %0 -> R1
    """)

    """%entry:
        %1 = add 6 17
        store %1 -> R1
    """
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
    %entry:
        store 23 -> R1
    """)

    """%entry:
    %0 = load R0
    %1 = add %0 %0
    store %1 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R0
    %1 = shl %0
    store %1 -> R1
    """)

[<Fact>]
let ``Instruction Simplify: Shift patterns`` () =
    """%entry:
    %0 = shl 3
    store %0 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    store 6 -> R1
    """)

    """%entry:
    %0 = shr 3
    store %0 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    store 1 -> R1
    """)

[<Fact>]
let ``Instruction Simplify: Not patterns`` () =

    """%entry:
    %0 = not 3
    store %0 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    store -4 -> R1
    """)

[<Fact>]
let ``Instruction Simplify: Branch patterns`` () =
    """%entry:
    br 0 = 0 %true %false
%true:
    store 3 -> R0
%false:
    store 5 -> R0
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    goto %true
%true:
    store 3 -> R0
%false:
    store 5 -> R0
    """)

    """%entry:
    br 0 < 0 %true %false
%true:
    store 3 -> R0
%false:
    store 5 -> R0
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    goto %false
%true:
    store 3 -> R0
%false:
    store 5 -> R0
    """)

    """%entry:
    br 0 < 0 %true %true
%true:
    store 3 -> R0
%false:
    store 5 -> R0
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    goto %true
%true:
    store 3 -> R0
%false:
    store 5 -> R0
    """)

[<Fact>]
let ``Instruction Simplify: Phi Instruction`` () =
    """%entry:
    %0 = load R0
    br %0 = 0 %true %false
%true:
    goto %cont
%false:
    goto %cont
%cont:
    %1 = phi (1,%true) (1,%false)
    store %1 -> R0
"""
    |> IRReader.fromString
    |> Passes.instructionSimplify
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R0
    br %0 = 0 %true %false
%true:
    goto %cont
%false:
    goto %cont
%cont:
    store 1 -> R0
    """)

[<Fact>]
let ``Instruction Combine`` () =
    """%entry:
    %0 = load R1
    %1 = not %0
    %2 = not %1
    store %2 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionCombine
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = not %0
    store %0 -> R1
    """)

    """%entry:
    %0 = load R1
    %1 = add 1 %0
    %2 = add 3 %1
    store %2 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionCombine
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = add 4 %0
    store %1 -> R1
    """)

    """%entry:
    %0 = load R1
    %1 = and 1 %0
    %2 = and 3 %1
    store %2 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionCombine
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = and 3 %0
    store %1 -> R1
    """)

    """%entry:
    %0 = load R1
    %1 = and %0 -32768
    br %1 = 0 %true %false
%true:
    store 0 -> R1
%false:
    store 1 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionCombine
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    br %0 < 0 %false %true
%true:
    store 0 -> R1
%false:
    store 1 -> R1
    """)

    """%entry:
    %0 = load R1
    %1 = not %0
    %2 = and %1 %0
    store %2 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionCombine
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = not %0
    store 0 -> R1
    """)

    """%entry:
    %0 = load R1
    %1 = not %0
    %2 = add %1 %0
    store %2 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionCombine
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = not %0
    store -1 -> R1
    """)

    """%entry:
    %0 = load R1
    %1 = not %0
    %2 = add %1 1
    %3 = add %2 %0
    store %3 -> R1
"""
    |> IRReader.fromString
    |> Passes.instructionCombine
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    %1 = not %0
    %2 = add %1 1
    store 0 -> R1
    """)

[<Fact>]
let ``Dead code elimination`` () =
    """
%entry:
    %0 = load R1
    %1 = not %0
    %2 = add %1 1
    store 0 -> R1
    """
    |> IRReader.fromString
    |> Passes.deadCodeElimination
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    store 0 -> R1
""")

[<Fact>]
let ``Simplify Control Flow Graph`` () =
    """
%entry:
    %0 = load R1
    goto %next

%next:
    store %0 -> R2
    """
    |> IRReader.fromString
    |> Passes.simplifyCFG
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    store %0 -> R2
    """)

    """
%entry:
    %0 = load R1
    br %0 = 0 %true %next

%true:
    store 0 -> R2

%next:
    goto %false

%false:
    store 1 -> R2
    """
    |> IRReader.fromString
    |> Passes.simplifyCFG
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    br %0 = 0 %true %false

%true:
    store 0 -> R2

%false:
    store 1 -> R2
    """)

[<Fact>]
let ``Jump threading`` () =
    """
%entry:
    %0 = load R1
    br %0 = 0 %isZero %isNotZero

%isZero:
    goto %boolCont

%isNotZero:
    goto %boolCont

%boolCont:
    %1 = phi (0,%isZero) (1,%isNotZero)
    %2 = load R0
    br %1 = 0 %isTrue %isFalse

%isTrue:
    goto %final

%isFalse:
    goto %final

%final:
    %3 = phi (0,%isTrue) (%2,%isFalse)
    store %3 -> PC
    """
    |> IRReader.fromString
    |> Passes.jumpThreading
    |> should
        be
           (structurallyEquivalentTo """
%entry:
	%0 = load R1
	br %0 = 0 %isZero %isNotZero

%isZero:
	goto %boolCont.copy

%boolCont.copy:
	%1 = load R0
	br 0 = 0 %isTrue.copy %isFalse.copy

%isFalse.copy:
	goto %final.copy

%final.copy:
	store %1 -> PC

%isTrue.copy:
	goto %final.copy0

%final.copy0:
	store 0 -> PC

%isNotZero:
	goto %boolCont.copy0

%boolCont.copy0:
	%2 = load R0
	br 1 = 0 %isTrue.copy0 %isFalse.copy0

%isFalse.copy0:
	goto %final.copy1

%final.copy1:
	store %2 -> PC

%isTrue.copy0:
	goto %final.copy2

%final.copy2:
	store 0 -> PC
    """)

    """
%entry:
    %0 = load R1
    br %0 = 0 %isZero %isNotZero

%isZero:
    goto %boolCont

%isNotZero:
    goto %boolCont

%boolCont:
    %1 = phi (0,%isZero) (1,%isNotZero)
    store %1 -> R1
    """
    |> IRReader.fromString
    |> Passes.jumpThreading
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    br %0 = 0 %isZero %isNotZero

%isZero:
    goto %boolCont.copy0

%boolCont.copy0:
    store 0 -> R1

%isNotZero:
    goto %boolCont.copy1

%boolCont.copy1:
    store 1 -> R1

    """)


[<Fact>]
let ``BasicBlock reordering`` () =
    """%entry:
    %0 = load R1
    br %0 = 0 %isZero %isNotZero

%isZero:
    store 1 -> R2

%isNotZero:
    store 0 -> R2
"""
    |> IRReader.fromString
    |> Passes.numberAll
    |> Passes.reorderBasicBlocks
    |> should
        be
           (structurallyEquivalentTo """
%entry:
    %0 = load R1
    br %0 = 0 %isZero %isNotZero

%isNotZero:
    store 0 -> R2

%isZero:
    store 1 -> R2

    """)
