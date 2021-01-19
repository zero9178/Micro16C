module Micro16C.Backend.RegisterAllocator

open System
open Micro16C.MiddleEnd
open Micro16C.MiddleEnd.IR
open Micro16C.MiddleEnd.Util

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

let allocateRegisters irModule =

    !irModule
    |> Module.entryBlock
    |> Option.map
        (Graphs.preOrder
            ((!)
             >> Value.asBasicBlock
             >> BasicBlock.immediatelyDominates
             >> Seq.ofList))
    |> Option.defaultValue Seq.empty
    |> Seq.iter (fun bb ->
        let block = !bb |> Value.asBasicBlock
        let assigned = Array.create 13 false

        block.LiveIn
        |> Seq.iter
            ((!)
             >> Value.register
             >> Option.map registerToIndex
             >> Option.iter (fun i -> Array.set assigned i true))

        block
        |> BasicBlock.instructions
        |> Seq.iter (fun value ->

            let isLastUse operand =
                if block.LiveOut |> ImmutableSet.contains operand then
                    false
                else
                    block
                    |> BasicBlock.revInstructions
                    |> Seq.map
                        (associateWith
                            ((!)
                             >> Value.operands
                             >> Seq.filter ((!) >> Value.isInstruction)))
                    |> Seq.find (snd >> Seq.contains operand)
                    |> fst = value

            match !value with
            | { Content = PhiInstruction _ } -> ()
            | _ ->
                !value
                |> Value.operands
                |> Seq.filter ((!) >> Value.isInstruction)
                |> Seq.filter isLastUse
                |> Seq.iter (fun value ->
                    Array.set
                        assigned
                        (!value
                         |> Value.register
                         |> Option.get
                         |> registerToIndex)
                        false)

            if !value |> Value.producesValue then
                match Array.tryFindIndex (id >> not) assigned with
                | None -> failwith "Register Pressure too high, spilling is not yet implemented"
                | Some i ->
                    Array.set assigned i true

                    value
                    := { !value with
                             Register = indexToRegister i |> Some }))

    irModule
