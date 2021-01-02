module Micro16C.Backend.RegisterAllocator

open System.Collections.Generic
open System.Collections.Immutable
open Micro16C.MiddleEnd.IR

let allocateRegisters (irModule: Module): ImmutableDictionary<Value ref, Register> =

    let adjustForPhi i instr (map: ImmutableDictionary<Value ref, int * int>) =
        match !instr with
        | { ParentBlock = Some parentBlock } as instrValue when Value.isTerminating instrValue ->
            // Check successors if they have phi values. If they do we need to extend the lifetime of the value
            // coming from this block to the terminating instruction itself
            !parentBlock
            |> BasicBlock.successors
            |> List.fold (fun map succ ->
                !succ
                |> Value.asBasicBlock
                |> BasicBlock.instructions
                |> List.takeWhile (fun x ->
                    match !x with
                    | { Content = PhiInstruction _ } -> true
                    | _ -> false)
                |> List.fold (fun (map: ImmutableDictionary<Value ref, int * int>) phi ->
                    match !phi with
                    | { Content = PhiInstruction { Incoming = list } } ->
                        let (op, _) =
                            list |> List.find (snd >> ((=) parentBlock))

                        if Value.producesValue !op then
                            match map.TryGetValue op with
                            | (false, _) -> map.Add(op, (i, i))
                            | (true, (start, _)) -> map.SetItem(op, (start, i))
                        else
                            map
                    | _ -> failwith "Internal Compiler Error: Expected Phi Instruction") map) map
        | _ -> map

    let liveIntervals =
        irModule
        |> Module.instructions
        |> List.indexed
        |> List.fold (fun (map: ImmutableDictionary<Value ref, int * int>) (i, instr) ->
            let map =
                match !instr with
                | { Content = AllocationInstruction _ } -> map
                | _ ->
                    let used =
                        if Value.producesValue !instr then [ instr ] else []
                        @ match !instr with
                          | { Content = PhiInstruction _ } -> []
                          | _ ->
                              !instr
                              |> Value.operands
                              |> List.filter ((!) >> Value.producesValue)

                    used
                    |> List.fold (fun map instr ->
                        match map.TryGetValue instr with
                        | (false, _) -> map.Add(instr, (i, i))
                        | (true, (start, _)) -> map.SetItem(instr, (start, i))) map

            adjustForPhi i instr map) (ImmutableDictionary.Create<Value ref, int * int>(HashIdentity.Reference))

    let registerToIndex register =
        match register with
        | R0 -> 0
        | R1 -> 1
        | R2 -> 2
        | R3 -> 3
        | R4 -> 4
        | R5 -> 5
        | R6 -> 6
        | R7 -> 7
        | R8 -> 8
        | R9 -> 9
        | R10 -> 10
        | AC -> 11
        | PC -> 12

    let explicitlyAllocatedRegisters =
        liveIntervals
        |> Seq.choose (fun kvp ->
            match !kvp.Key with
            | { Content = PhiInstruction { Register = Some r } } -> Some(registerToIndex r, kvp.Value)
            | _ -> None)
        |> Map.ofSeq

    let explicitlyAllocatedValues =
        liveIntervals
        |> Seq.choose (fun kvp ->
            match !kvp.Key with
            | { Content = PhiInstruction { Register = Some r } } -> KeyValuePair(kvp.Key, registerToIndex r) |> Some
            | _ -> None)
        |> fun x -> ImmutableDictionary.CreateRange(HashIdentity.Reference, x)

    let valueStarts =
        liveIntervals
        |> Seq.map (fun kv ->
            let (x, (start, _)) = kv.Deconstruct()
            (start, x))
        |> Seq.groupBy fst
        |> Seq.map (fun (i, x) -> (i, x |> Seq.map snd |> List.ofSeq))
        |> Map.ofSeq

    let valueEnds =
        liveIntervals
        |> Seq.map (fun kv ->
            let (x, (_, endV)) = kv.Deconstruct()
            KeyValuePair(x, endV))
        |> (fun x -> ImmutableDictionary.CreateRange(HashIdentity.Reference, x))

    let indexToRegister index =
        match index with
        | 0 -> R0
        | 1 -> R1
        | 2 -> R2
        | 3 -> R3
        | 4 -> R4
        | 5 -> R5
        | 6 -> R6
        | 7 -> R7
        | 8 -> R8
        | 9 -> R9
        | 10 -> R10
        | 11 -> AC
        | 12 -> PC
        | _ -> failwith "Internal Compiler Error: Invalid Index"

    let inUseRegisters = Array.init 13 (fun _ -> None)

    let findRegisterFor inUseRegisters v =

        let intervalsOverlap i1 i2 =
            ((fst i1) >= (fst i2) && (fst i1) < (snd i2))
            || (snd i1) >= (fst i2) && (snd i1) < (snd i2)

        let rec findRegisterForImpl index (array: int option []) =
            match Array.tryItem index array with
            | None -> None
            | Some (Some _) -> findRegisterForImpl (index + 1) array
            | Some None ->
                match Map.tryFind index explicitlyAllocatedRegisters with
                | Some interval1 when intervalsOverlap interval1 (liveIntervals.[v]) ->
                    findRegisterForImpl (index + 1) array
                | _ -> Some index

        findRegisterForImpl 0 inUseRegisters

    irModule
    |> Module.instructions
    |> List.indexed
    |> List.map fst
    |> List.fold (fun (map, inUseRegisters) i ->
        let inUseRegisters =
            inUseRegisters
            |> Array.map (fun x ->
                match x with
                | Some v when v = i -> None
                | _ -> x)

        match Map.tryFind i valueStarts with
        | None -> (map, inUseRegisters)
        | Some values ->
            values
            |> Seq.fold (fun ((map, inUseRegisters): ImmutableDictionary<Value ref, Register> * int option []) v ->

                let index =
                    match explicitlyAllocatedValues.TryGetValue v with
                    | (false, _) ->
                        match findRegisterFor inUseRegisters v with
                        | None ->
                            failwith
                                "Register Allocation Failure. Too many values alive at once. Not enough registers left"
                        | Some index -> index
                    | (true, index) ->
                        assert (Option.isNone inUseRegisters.[index])
                        index

                Array.set inUseRegisters index (valueEnds.[v] |> Some)
                (map.Add(v, indexToRegister index), inUseRegisters)) (map, inUseRegisters))
           (ImmutableDictionary.Create<Value ref, Register>(HashIdentity.Reference), inUseRegisters)
    |> fst
