module Micro16C.Backend.RegisterAllocator

open System.Collections.Generic
open System.Collections.Immutable
open Micro16C.MiddleEnd.IR

let allocateRegisters (irModule: Module): ImmutableDictionary<Value ref, Register> =

    let liveIntervals =
        irModule
        |> Module.instructions
        |> List.indexed
        |> List.fold (fun (map: ImmutableDictionary<Value ref, int * int>) (i, instr) ->

            let i, used =
                match !instr with
                | { Content = PhiInstruction _
                    ParentBlock = Some parentBlock } ->
                    (i
                     + (!parentBlock
                        |> Value.asBasicBlock
                        |> BasicBlock.phis
                        |> List.skipWhile ((<>) instr)
                        |> List.length),
                     [ instr ])
                | _ ->
                    (i,
                     (if Value.producesValue !instr then [ instr ] else [])
                     @ (!instr
                        |> Value.operands
                        |> List.filter ((!) >> Value.producesValue)))

            match !instr with
            | { Content = AllocationInstruction _ } -> map
            | _ ->
                used
                |> List.fold (fun map instr ->
                    match map.TryGetValue instr with
                    | (false, _) -> map.Add(instr, (i, i))
                    | (true, (start, _)) -> map.SetItem(instr, (start, i))) map

            ) (ImmutableDictionary.Create<Value ref, int * int>(HashIdentity.Reference))

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

    let findRegisterFor inUseRegisters v =

        let intervalsOverlap i1 i2 =
            ((fst i1) >= (fst i2) && (fst i1) < (snd i2))
            || (fst i2) >= (fst i1) && (fst i2) < (snd i1)

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

                match valueEnds.[v] with
                | endV when endV = i -> ()
                | endV -> Array.set inUseRegisters index (endV |> Some)

                (map.Add(v, indexToRegister index), inUseRegisters)) (map, inUseRegisters))
           (ImmutableDictionary.Create<Value ref, Register>(HashIdentity.Reference), Array.init 13 (fun _ -> None))
    |> fst
