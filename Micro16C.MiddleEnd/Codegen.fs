module Micro16C.MiddleEnd.Codegen

open Micro16C.Frontend
open Micro16C.Frontend.Lex
open Micro16C.Frontend.ErrorHandling
open Micro16C.MiddleEnd.IR

type Context =
    { Builder: Builder
      Variables: Map<Sema.Declaration, Value ref>
      Labels: Map<string * Sema.Statement, Value ref> }

module private Context =

    let private withBuilder context builder = { context with Builder = builder }

    let private retWithBuilder context (value, builder) = (value, withBuilder context builder)

    let createBasicBlock name context =
        Builder.createBasicBlock name context.Builder
        |> retWithBuilder context

    let insertPoint (context: Context) = context.Builder.InsertBlock

    let setInsertPoint basicBlock context =
        context.Builder
        |> Builder.setInsertBlock basicBlock
        |> withBuilder context

    let createNamedAlloca name context =
        context.Builder
        |> Builder.createNamedAlloca name
        |> retWithBuilder context

    let createAlloca = createNamedAlloca ""

    let createNamedBinary name left kind right context =
        context.Builder
        |> Builder.createNamedBinary name left kind right
        |> retWithBuilder context

    let createBinary = createNamedBinary ""

    let createNamedUnary name kind value context =
        context.Builder
        |> Builder.createNamedUnary name kind value
        |> retWithBuilder context

    let createUnary = createNamedUnary ""

    let createNamedLoad name value context =
        context.Builder
        |> Builder.createNamedLoad name value
        |> retWithBuilder context

    let createLoad = createNamedLoad ""

    let createStore destination value context =
        context.Builder
        |> Builder.createStore destination value
        |> snd
        |> withBuilder context

    let createGoto destination context =
        context.Builder
        |> Builder.createGoto destination
        |> snd
        |> withBuilder context

    let createCondBr kind condition trueBranch falseBranch context =
        context.Builder
        |> Builder.createCondBr kind condition trueBranch falseBranch
        |> snd
        |> withBuilder context

    let createNamedPhi name incoming context =
        context.Builder
        |> Builder.createNamedPhi name incoming
        |> retWithBuilder context

    let createPhi = createNamedPhi ""

    let builder context = context.Builder

module private Op =

    let negate value context =
        (value, context)
        ||> Context.createUnary Not
        ||> Context.createBinary (Builder.createConstant 1s) Add

    let plus lhs rhs context = Context.createBinary lhs Add rhs context

    let minus lhs rhs context = (rhs, context) ||> negate ||> plus lhs

    let bitAnd lhs rhs context = Context.createBinary lhs And rhs context

    let bitNot value context =
        (value, context) ||> Context.createUnary Not

    let bitOr lhs rhs context =
        // R0 | R1 = ~(~R0 & ~R1)
        let lhs, context = bitNot lhs context
        (rhs, context) ||> bitNot ||> bitAnd lhs

    let bitXor lhs rhs context =
        // R0 ^ R1 = ~(~(~(R0 & R1) & R0) & ~(~(R0 & R1) & R1))

        let midAnd, context = bitAnd lhs rhs context ||> bitNot
        let lhs, context = bitAnd lhs midAnd context ||> bitNot
        let rhs, context = bitAnd rhs midAnd context ||> bitNot
        bitAnd lhs rhs context ||> bitNot

    let rem lhs rhs context =

        let neg, context =
            Context.createBasicBlock "modNeg" context

        match Context.insertPoint context with
        | None ->
            // if we are in dead code, this is all irrelevant and impossible to implement
            (Builder.createConstant 0s, context)
        | Some prev ->
            let cont, context = Context.createBasicBlock "cont" context

            let negated, context =
                context
                |> Context.createCondBr Negative lhs neg cont
                |> Context.setInsertPoint (Some neg)
                |> pack2 lhs
                ||> negate

            let context =
                context
                |> Context.createGoto cont
                |> Context.setInsertPoint (Some cont)

            let phi, context =
                context
                |> Context.createPhi [ (negated, neg)
                                       (lhs, prev) ]

            let acc, context = Context.createAlloca context
            let context = context |> Context.createStore acc phi

            let rhs, context = (rhs, context) ||> negate

            let body, context =
                Context.createBasicBlock "modBody" context

            let context =
                context
                |> Context.createGoto body
                |> Context.setInsertPoint (Some body)


            let value, context =
                (acc, context) ||> Context.createLoad ||> plus rhs

            let modCont, context =
                Context.createBasicBlock "modCont" context

            let modEnd, context =
                Context.createBasicBlock "modEnd" context

            context
            |> Context.createCondBr Negative value modEnd modCont
            |> Context.setInsertPoint (Some modCont)
            |> Context.createStore acc value
            |> Context.createGoto body
            |> Context.setInsertPoint (Some modEnd)
            |> Context.createLoad acc

let rec visitCompoundStatement (compoundItems: Sema.CompoundItem list) (context: Context) =
    compoundItems
    |> List.fold (fun context x ->
        match x with
        | Sema.CompoundItemStatement statement -> context |> visitStatement statement
        | Sema.CompoundItemDeclaration declaration ->
            declaration
            |> List.fold (fun context x -> visitDeclaration x context) context) context

and visitStatement (statement: Sema.Statement) (context: Context): Context =
    match statement with
    | Sema.IfStatement (condition, trueStatement, falseStatement) ->
        let condition, context = visitExpression condition context

        let trueBranch, context =
            Context.createBasicBlock "ifTrue" context

        let continueBranch, context =
            Context.createBasicBlock "ifCond" context

        let falseBranch, context =
            match falseStatement with
            | None -> (continueBranch, context)
            | Some _ -> Context.createBasicBlock "ifFalse" context

        let context =
            context
            |> Context.createCondBr Zero condition falseBranch trueBranch
            |> Context.setInsertPoint (Some trueBranch)
            |> visitStatement trueStatement

        let trueBranch = Context.insertPoint context

        let context =
            context |> Context.createGoto continueBranch

        let falseBranch, context =
            match falseStatement with
            | None -> (falseBranch |> Some, context)
            | Some falseStatement ->
                let context =
                    context
                    |> Context.setInsertPoint (Some falseBranch)
                    |> visitStatement falseStatement

                let falseBranch = Context.insertPoint context

                (falseBranch, context |> Context.createGoto continueBranch)

        match (trueBranch, falseBranch) with
        | (Some _, _)
        | (_, Some _) ->
            context
            |> Context.setInsertPoint (Some continueBranch)
        | _ -> context
    | Sema.DoWhileStatement statement ->
        let body, context =
            Context.createBasicBlock "doWhileBody" context

        let value, context =
            context
            |> Context.createGoto body
            |> Context.setInsertPoint (Some body)
            |> visitStatement statement.Statement
            |> visitExpression statement.Expression

        let cont, context =
            Context.createBasicBlock "doWhileContinue" context

        context
        |> Context.createCondBr Zero value cont body
        |> Context.setInsertPoint (Some cont)
    | Sema.WhileStatement statement ->
        let cond, context = Context.createBasicBlock "cond" context

        let value, context =
            context
            |> Context.createGoto cond
            |> Context.setInsertPoint (Some cond)
            |> visitExpression statement.Expression

        let cont, context =
            Context.createBasicBlock "WhileContinue" context

        let body, context =
            Context.createBasicBlock "WhileBody" context

        context
        |> Context.createCondBr Zero value cont body
        |> Context.setInsertPoint (Some body)
        |> visitStatement statement.Statement
        |> Context.createGoto cond
        |> Context.setInsertPoint (Some cont)
    | Sema.CompoundStatement statement -> visitCompoundStatement statement context
    | Sema.ExpressionStatement None -> context
    | Sema.ExpressionStatement (Some expression) -> context |> visitExpression expression |> snd
    | Sema.GotoStatement statement ->
        match !statement with
        | None -> failwith "Internal Compiler Error: No corresponding label for Goto"
        | Some label ->
            (match Map.tryFind label context.Labels with
             | None ->
                 let bb, context =
                     Context.createBasicBlock (fst label) context

                 { context with
                       Labels = Map.add label bb context.Labels }
                 |> pack2 bb
             | Some bb -> (bb, context))
            ||> Context.createGoto
            |> Context.setInsertPoint None

    | Sema.LabelStatement (label, statement) ->
        let bb, context =
            (match Map.tryFind (label, statement) context.Labels with
             | None ->
                 let bb, context = Context.createBasicBlock label context

                 (bb,
                  { context with
                        Labels = Map.add (label, statement) bb context.Labels })
             | Some bb -> (bb, context))

        context
        |> Context.createGoto bb
        |> Context.setInsertPoint (Some bb)
        |> visitStatement statement
    | _ -> failwith "TODO"

and visitDeclaration (declaration: Sema.Declaration) (context: Context): Context =
    let value, context =
        Context.createNamedAlloca (Token.identifier declaration.Name) context

    let context =
        { context with
              Variables = Map.add declaration value context.Variables }


    match declaration.Initializer with
    | None -> context
    | Some initializer ->
        visitExpression initializer context
        ||> Context.createStore value

and visitBinaryExpression (expression: Sema.Binary) (context: Context) =
    let lhs, context = visitExpression expression.Left context
    let rhs, context = visitExpression expression.Right context

    match expression.Kind with
    | Sema.Plus -> Op.plus lhs rhs context
    | Sema.Minus -> Op.minus lhs rhs context
    | Sema.BitAnd -> Op.bitAnd lhs rhs context
    | Sema.BitOr -> Op.bitOr lhs rhs context
    | Sema.BitXor -> Op.bitXor lhs rhs context
    | Sema.SubScript -> Op.plus lhs rhs context ||> Context.createLoad
    | Sema.Equal -> Op.bitXor lhs rhs context ||> Op.bitNot
    | Sema.NotEqual -> Op.bitXor lhs rhs context
    | Sema.Modulo -> Op.rem lhs rhs context
    | _ -> failwith "TODO"

and visitAssignmentExpression (expression: Sema.Assignment) (context: Context) =
    let lhs, context =
        visitExpression expression.LValue context

    let rhs, context =
        visitExpression expression.Expression context

    match expression.Kind with
    | Sema.Normal ->
        context
        |> Context.createStore lhs rhs
        |> Context.createLoad lhs
    | Sema.PlusAssign ->
        context
        |> Context.createLoad lhs
        ||> Op.plus rhs
        ||> Context.createStore lhs
        |> Context.createLoad lhs
    | Sema.MinusAssign ->
        let value, context = context |> Context.createLoad lhs

        (rhs, context)
        ||> Op.minus value
        ||> Context.createStore lhs
        |> Context.createLoad lhs
    | Sema.BitAndAssign ->
        context
        |> Context.createLoad lhs
        ||> Op.bitAnd rhs
        ||> Context.createStore lhs
        |> Context.createLoad lhs
    | Sema.BitOrAssign ->
        context
        |> Context.createLoad lhs
        ||> Op.bitOr rhs
        ||> Context.createStore lhs
        |> Context.createLoad lhs
    | Sema.BitXorAssign ->
        context
        |> Context.createLoad lhs
        ||> Op.bitXor rhs
        ||> Context.createStore lhs
        |> Context.createLoad lhs
    | _ -> failwith "TODO"

and visitExpression (expression: Sema.Expression) (context: Context) =
    match expression with
    | Sema.ReferenceExpression expression ->
        let value =
            Map.find expression.Declaration context.Variables

        (value, context)
    | Sema.BinaryExpression expression -> visitBinaryExpression expression context
    | Sema.ConversionExpression expression ->
        visitExpression expression.Expression context
        ||> Context.createLoad
    | Sema.UnaryExpression expression ->
        let value, context =
            visitExpression expression.Expression context

        match expression.Kind with
        | Sema.AddressOf -> (value, context)
        | Sema.Dereference -> Context.createLoad value context
        | Sema.UnaryOperator.Minus ->
            (value, context)
            ||> Context.createUnary Not
            ||> Context.createBinary (Builder.createConstant 1s) Add
        | Sema.BitwiseNegate -> Context.createUnary Not value context
        | _ -> failwith "TODO"
    | Sema.SizeofExpression _ -> (Builder.createConstant 1s, context)
    | Sema.CommaExpression expression ->
        let values, context =
            expression.Expressions
            |> List.fold (fun (result, context) x ->
                let value, context = visitExpression x context
                (value :: result, context)) ([], context)

        (List.head values, context)
    | Sema.ConstantExpression constant -> (Builder.createConstant constant.Value, context)
    | Sema.AssignmentExpression assignment -> visitAssignmentExpression assignment context
    | Sema.RegisterExpression { Type = tokenType } ->
        let register =
            match tokenType with
            | R0Keyword -> R0
            | R1Keyword -> R1
            | R2Keyword -> R2
            | R3Keyword -> R3
            | R4Keyword -> R4
            | R5Keyword -> R5
            | R6Keyword -> R6
            | R7Keyword -> R7
            | R8Keyword -> R8
            | R9Keyword -> R9
            | R10Keyword -> R10
            | ACKeyword -> AC
            | PCKeyword -> PC
            | _ -> failwith "Internal Compiler Error: Not a valid token type"

        (Builder.createRegister register, context)
    | _ -> failwith "TODO"

let codegen (translationUnit: Sema.CompoundItem list) =
    let context =
        { Builder = Builder.Default
          Variables = Map([])
          Labels = Map([]) }

    let entry, context = Context.createBasicBlock "entry" context


    context
    |> Context.setInsertPoint (Some entry)
    |> visitCompoundStatement translationUnit
    |> Context.builder
    |> Builder.finalize
