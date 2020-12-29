module Micro16C.MiddleEnd.Passes

open Micro16C.MiddleEnd.IR


let instructionSimplify (irModule: Module) =
    let simplify value =
        match !value with
        | { Content = BinaryInstruction { Kind = And
                                          Left = Ref { Content = Constant { Value = 0s } }
                                          Right = _ } }
        | { Content = BinaryInstruction { Kind = And
                                          Right = Ref { Content = Constant { Value = 0s } }
                                          Left = _ } } ->
            value
            |> Value.replaceWith (Builder.createConstant 0s)
        | { Content = BinaryInstruction { Kind = Add
                                          Left = Ref { Content = Constant { Value = 0s } }
                                          Right = passThrough } }
        | { Content = BinaryInstruction { Kind = Add
                                          Right = Ref { Content = Constant { Value = 0s } }
                                          Left = passThrough } } -> value |> Value.replaceWith passThrough
        | { Content = BinaryInstruction { Kind = And
                                          Left = Ref { Content = Constant { Value = 0xFFFFs } }
                                          Right = passThrough } }
        | { Content = BinaryInstruction { Kind = And
                                          Right = Ref { Content = Constant { Value = 0xFFFFs } }
                                          Left = passThrough } } -> value |> Value.replaceWith passThrough
        | { Content = BinaryInstruction { Kind = Add
                                          Left = Ref { Content = Constant { Value = lhs } }
                                          Right = Ref { Content = Constant { Value = rhs } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs + rhs))
        | { Content = BinaryInstruction { Kind = And
                                          Left = Ref { Content = Constant { Value = lhs } }
                                          Right = Ref { Content = Constant { Value = rhs } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant (lhs &&& rhs))
        | { Content = UnaryInstruction { Kind = Not
                                         Value = Ref { Content = Constant { Value = rhs } } } } ->
            value
            |> Value.replaceWith (Builder.createConstant (~~~rhs))
        | _ -> ()

    irModule
    |> Module.instructions
    |> Seq.iter simplify

    irModule

let deadCodeElimination (irModule: Module) =

    let eliminate value =
        if not (Value.hasSideEffects !value)
           && 0 = Value.useCount !value then
            value |> Value.eraseFromParent

    irModule
    |> Module.instructions
    |> Seq.iter eliminate

    irModule

let simplifyCFG (irModule: Module) =

    let simplifyBlock blockValue =
        match !blockValue with
        | { Content = BasicBlockValue block } ->
            match block.Instructions with
            | [ Ref { Content = GotoInstruction { BasicBlock = destination } } ] ->
                blockValue |> Value.replaceWith destination
                false
            | _ -> true
        | _ -> failwith "Internal Compiler Error"

    let blocks =
        irModule
        |> Module.basicBlocks
        |> Seq.filter simplifyBlock
        |> List.ofSeq

    { BasicBlocks = blocks }
