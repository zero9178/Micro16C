module Micro16C.Backend.RegisterAllocator

open System
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util

let private registerToIndex register =
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


let private indexToRegister index =
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

type private RegisterPlan =
    | SuspendAt of int
    | RestartAt of int
    | KillAt of int

module private RegisterPlan =

    let value =
        function
        | RestartAt i
        | KillAt i
        | SuspendAt i -> i

let rec private intervalsFitIn (intervals: (int * int) list list) (newIntervals: (int * int) list) =
    let overlap (lhs: int * int) (rhs: int * int) =
        ((fst lhs) >= (fst rhs) && (fst lhs) < (snd rhs))
        || ((fst rhs) >= (fst lhs) && (fst rhs) < (snd lhs))

    intervals
    |> List.exists (fun other ->
        Seq.unfold (fun (other, intervals, result) ->
            match (other, intervals, result) with
            | _, _, true
            | [], _, _
            | _, [], _ -> None
            | otherHead :: _, headInterval :: _, _ ->
                let other, intervals =
                    if otherHead < headInterval then
                        (other
                         |> List.skipWhile (snd >> (>=) (fst headInterval)),
                         intervals)
                    else
                        (other,
                         intervals
                         |> List.skipWhile (snd >> (>=) (fst otherHead)))

                match List.tryHead other, List.tryHead intervals with
                | _, None -> Some(false, (other, intervals, false))
                | None, _ -> Some(false, (other, intervals, false))
                | Some otherHead, Some headInterval ->
                    if overlap headInterval otherHead
                    then Some(true, (other, intervals, true))
                    else if otherHead < headInterval
                    then Some(false, (other |> List.tail, intervals, false))
                    else Some(false, (other, intervals |> List.tail, false))) (other, newIntervals, false)
        |> Seq.tryLast
        |> Option.defaultValue false)
    |> not


let allocateRegisters irModule =
    let lifeIntervals =
        !irModule
        |> Module.revInstructions
        |> List.map (associateWith ((!) >> Value.lifeIntervals))
        |> List.filter (snd >> List.isEmpty >> not)
        // Don't allocate registers for values whose only usage is in conditional branches, these can be be folded
        // and done in parallel in Assembly. Let's only do so though if the conditional branch is right after the
        // the definition of the condition. A separate pass should be responsible to increasing data locality if need be.
        //
        // Exception to the above is shift operations as well as Phis. The Control Unit of the Micro16 reads from the
        // ALU, the shifter is separate and placed after the ALU. Results from shift operations need to be stored in a
        // register. Phis because they are not actually real processor instructions but copies generate in predecessor
        // blocks
        |> List.filter (function
            | Ref { Content = UnaryInstruction { Kind = kind } }, _ when kind = Shl || kind = Shr -> true
            | Ref { Content = PhiInstruction _ }, _ -> true
            | Ref { Users = [ Ref { Content = CondBrInstruction _
                                    Index = Some condIndex
                                    ParentBlock = Some condParent } ]
                    Index = Some index
                    ParentBlock = Some parent },
              _ when condIndex = index + 1 && parent = condParent -> false
            | _ -> true)
        |> ImmutableMap.ofList

    let valueStarts =
        lifeIntervals
        |> Seq.map (fun kv ->
            let value, list = kv.Deconstruct()

            let plan =
                list
                |> List.fold (fun result (first, second) ->
                    match result with
                    | [] -> [ SuspendAt(second) ]
                    | SuspendAt (value) :: tail when value = first -> SuspendAt(second) :: tail
                    | _ when second <> first -> SuspendAt(second) :: RestartAt(first) :: result
                    | _ -> result) []

            let plan =
                match plan with
                | SuspendAt value :: tail -> KillAt value :: tail
                | RestartAt value :: _ -> KillAt value :: plan
                | _ -> failwith "Ought to not be possible"

            (list |> List.head |> fst, (value, plan |> List.rev)))
        |> Seq.groupBy fst
        |> Seq.map (fun (x, y) -> (x, y |> Seq.map snd))
        |> Map.ofSeq

    let inUseRegisters = Array.init 13 (fun _ -> [])
    let lifeIntervalsOnRegister = Array.init 13 (fun _ -> [])

    let findFreeRegisterWithPref preference intervals =

        let isFree index =
            List.isEmpty inUseRegisters.[index]
            && intervals
               |> intervalsFitIn lifeIntervalsOnRegister.[index]

        let rec findFreeRegisterImpl startIndex =
            if startIndex >= Array.length inUseRegisters
            then None
            else if isFree startIndex
            then Some startIndex
            else findFreeRegisterImpl (startIndex + 1)

        match preference with
        | Some preference when preference |> isFree -> Some preference
        | Some _
        | None -> findFreeRegisterImpl 0

    let findFreeRegister = findFreeRegisterWithPref None

    !irModule
    |> Module.instructions
    |> List.indexed
    |> List.map fst
    |> List.fold (fun map i ->

        let map =
            inUseRegisters
            |> Array.indexed
            |> Array.filter
                (snd
                 >> List.tryHead
                 >> Option.map RegisterPlan.value
                 >> (=) (Some i))
            |> Array.fold (fun map (i, plan) ->
                match plan with
                | KillAt _ :: _ ->
                    Array.set inUseRegisters i []
                    map
                | SuspendAt _ :: RestartAt instr :: tail ->
                    Array.set inUseRegisters i []

                    match map |> Map.tryFind instr with
                    | None -> Map.add instr [ (i, tail) ] map
                    | Some list -> map |> Map.add instr ((i, tail) :: list)
                | _ -> map) map

        let map =
            map
            |> Map.tryFind i
            |> Option.map (fun list ->
                list
                |> List.iter (fun (i, plan) ->
                    assert (Array.get inUseRegisters i |> List.isEmpty)
                    Array.set inUseRegisters i plan)

                map |> Map.remove i)
            |> Option.defaultValue map

        match valueStarts |> Map.tryFind i with
        | None -> map
        | Some seq ->

            seq
            |> Seq.fold (fun map (value, plan) ->

                let result =
                    match !value with
                    | { Content = LoadInstruction { Source = Ref { Content = Register reg } } } ->
                        !value
                        |> Value.lifeIntervals
                        |> findFreeRegisterWithPref (reg |> registerToIndex |> Some)
                    | _ -> !value |> Value.lifeIntervals |> findFreeRegister

                match result with
                | None -> failwith "Too many registers alive at once. Spilling to memory is not yet implemented"
                | Some i ->
                    Array.set inUseRegisters i plan

                    value
                    := { !value with
                             Register = indexToRegister i |> Some }

                    (!value |> Value.lifeIntervals)
                    :: (Array.get lifeIntervalsOnRegister i)
                    |> Array.set lifeIntervalsOnRegister i

                    map) map

        ) (Map([]))
    |> ignore

    irModule
