module Micro16C.Frontend.Sema

open Micro16C.Frontend.Lex
open Micro16C.Frontend.ErrorHandling

type Type =
    | PrimitiveType of Primitive
    | PointerType of Pointer

and Primitive = | IntType

and Pointer = { ElementType: Type }

let createInt () = IntType |> PrimitiveType

let createPointer elementType =
    { ElementType = elementType } |> PointerType

type BinaryOperator =
    | Plus
    | Minus
    | Multiply
    | Division
    | Modulo
    | LogicOr
    | LogicAnd
    | BitOr
    | BitXor
    | BitAnd
    | Equal
    | NotEqual
    | LessThan
    | GreaterThan
    | LessThanOrEqual
    | GreaterThanOrEqual
    | ShiftLeft
    | ShiftRight
    | SubScript

type UnaryOperator =
    | PreIncrement
    | PostIncrement
    | PreDecrement
    | PostDecrement
    | AddressOf
    | Dereference
    | Minus
    | BitwiseNegate
    | LogicalNegate

type ValueKind =
    | LValue
    | RValue

type Expression =
    | BinaryExpression of Binary
    | UnaryExpression of Unary
    | SizeofExpression of Sizeof
    | CommaExpression of Comma
    | ConstantExpression of Constant
    | ReferenceExpression of Reference
    | ConversionExpression of Conversion

and Binary =
    { Type: Type
      ValueKind: ValueKind
      Left: Expression
      Right: Expression
      Kind: BinaryOperator }

and Unary =
    { Type: Type
      ValueKind: ValueKind
      Expression: Expression
      Kind: UnaryOperator }

and Sizeof = { Type: Type; Size: int }

and Comma =
    { Type: Type
      Expressions: Expression list }

and Constant = { Type: Type; Value: int16 }

and Reference =
    { Type: Type
      Declaration: Declaration }

and Conversion = { Type: Type; Expression: Expression }

and While =
    { Expression: Expression
      Statement: Statement }

and DoWhile =
    { Statement: Statement
      Expression: Expression }

and For =
    { Initial: Expression option
      Condition: Expression option
      Iteration: Expression option }

and LoopStatement =
    | WhileLoop of While
    | DoWhileLoop of DoWhile
    | ForLoop of For

and Statement =
    | WhileStatement of While
    | DoWhileStatement of DoWhile
    | ForStatement of For
    | BreakStatement of LoopStatement
    | ContinueStatement of LoopStatement
    | CompoundStatement of CompoundItem list
    | ExpressionStatement of Expression option

and Declaration =
    { Type: Type
      Name: Token
      Register: Token option
      Initializer: Expression option }

and CompoundItem =
    | CompoundItemStatement of Statement
    | CompoundItemDeclaration of Declaration list

module Expression =
    let getType (expr: Expression) =
        match expr with
        | BinaryExpression expr -> expr.Type
        | UnaryExpression expr -> expr.Type
        | SizeofExpression expr -> expr.Type
        | CommaExpression expr -> expr.Type
        | ConstantExpression expr -> expr.Type
        | ReferenceExpression expr -> expr.Type
        | ConversionExpression expr -> expr.Type

    let valueKind (expr: Expression) =
        match expr with
        | BinaryExpression expr -> expr.ValueKind
        | UnaryExpression expr -> expr.ValueKind
        | SizeofExpression _ -> RValue
        | CommaExpression _ -> RValue
        | ConstantExpression _ -> RValue
        | ReferenceExpression _ -> LValue
        | ConversionExpression _ -> RValue

let private (|Type|) (expression: Expression) = Expression.getType expression

let private (|ValueKind|) (expression: Expression) = Expression.valueKind expression

let private lvalueConversion expression =
    match Expression.valueKind expression with
    | RValue -> expression
    | LValue ->
        ConversionExpression
            { Type = Expression.getType expression
              Expression = expression }


type Context =
    { Scopes: Map<string, Declaration> list
      SourceObject: SourceObject }

let rec visitExpression (context: Context) (expression: Parse.Expression): Result<Expression, string> =
    match expression.OptionalAssignmentExpressions with
    | [] -> visitAssignmentExpression context expression.AssignmentExpression
    | list ->
        expression.AssignmentExpression :: list
        |> List.map (visitAssignmentExpression context)
        |> foldResults
        |> Result.map (fun list ->
            CommaExpression
                { Expressions = list
                  Type = list |> List.last |> Expression.getType })

and visitAssignmentExpression (context: Context) (expression: Parse.AssignmentExpression): Result<Expression, string> =
    match expression.OptionalConditionalExpressions with
    | [] -> visitConditionalExpression context expression.ConditionalExpression
    | list -> failwith "TODO"

and visitConditionalExpression (context: Context) (expression: Parse.ConditionalExpression): Result<Expression, string> =
    match expression.OptionalTernary with
    | None -> visitLogicalOrExpression context expression.LogicalOrExpression
    | Some ternary -> failwith "TODO"

and visitLogicalOrExpression (context: Context) (expression: Parse.LogicalOrExpression): Result<Expression, string> =
    match expression.OptionalLogicalAndExpressions with
    | [] -> visitLogicalAndExpression context expression.LogicalAndExpression
    | list -> failwith "TODO"

and visitLogicalAndExpression (context: Context) (expression: Parse.LogicalAndExpression): Result<Expression, string> =
    match expression.OptionalInclusiveOrExpressions with
    | [] -> visitInclusiveOrExpression context expression.InclusiveOrExpression
    | list -> failwith "TODO"

and visitInclusiveOrExpression (context: Context) (expression: Parse.InclusiveOrExpression): Result<Expression, string> =
    match expression.OptionalExclusiveOrExpressions with
    | [] -> visitExclusiveOrExpression context expression.ExclusiveOrExpression
    | list -> failwith "TODO"

and visitExclusiveOrExpression (context: Context) (expression: Parse.ExclusiveOrExpression): Result<Expression, string> =
    match expression.OptionalAndExpressions with
    | [] -> visitAndExpression context expression.AndExpression
    | list -> failwith "TODO"

and visitAndExpression (context: Context) (expression: Parse.AndExpression): Result<Expression, string> =
    match expression.OptionalEqualityExpressions with
    | [] -> visitEqualityExpression context expression.EqualityExpression
    | list -> failwith "TODO"

and visitEqualityExpression (context: Context) (expression: Parse.EqualityExpression): Result<Expression, string> =
    match expression.OptionalRelationalExpressions with
    | [] -> visitRelationalExpression context expression.RelationalExpression
    | list -> failwith "TODO"

and visitRelationalExpression (context: Context) (expression: Parse.RelationalExpression): Result<Expression, string> =
    match expression.OptionalShiftExpressions with
    | [] -> visitShiftExpression context expression.ShiftExpression
    | list -> failwith "TODO"

and visitShiftExpression (context: Context) (expression: Parse.ShiftExpression): Result<Expression, string> =
    match expression.OptionalAdditiveExpressions with
    | [] -> visitAdditiveExpression context expression.AdditiveExpression
    | list -> failwith "TODO"

and visitAdditiveExpression (context: Context) (expression: Parse.AdditiveExpression): Result<Expression, string> =
    match expression.OptionalMultiplicativeExpressions with
    | [] -> visitMultiplicativeExpression context expression.MultiplicativeExpression
    | list -> failwith "TODO"

and visitMultiplicativeExpression (context: Context)
                                  (expression: Parse.MultiplicativeExpression)
                                  : Result<Expression, string> =
    match expression.OptionalUnaryExpressions with
    | [] -> visitUnaryExpression context expression.UnaryExpression
    | list -> failwith "TODO"

and visitUnaryExpression (context: Context) (expression: Parse.UnaryExpression): Result<Expression, string> =
    match expression with
    | Parse.PostFixUnaryExpression expression -> visitPostFixExpression context expression
    | Parse.IncrementUnaryExpression (expression, token) ->
        let expression = visitUnaryExpression context expression

        match expression with
        | Ok (ValueKind RValue) ->
            "Cannot increment temporary value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | _ ->
            expression
            |> Result.map (fun expr ->
                UnaryExpression
                    { Type = Expression.getType expr
                      Kind = PreIncrement
                      Expression = expr
                      ValueKind = LValue })
    | Parse.DecrementUnaryExpression (expression, token) ->
        let expression = visitUnaryExpression context expression

        match expression with
        | Ok (ValueKind RValue) ->
            "Cannot decrement temporary value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | _ ->
            expression
            |> Result.map (fun expr ->
                UnaryExpression
                    { Type = Expression.getType expr
                      Kind = PreDecrement
                      Expression = expr
                      ValueKind = LValue })
    | Parse.AddressOfUnaryExpression (expression, token) ->
        let expression = visitUnaryExpression context expression

        match expression with
        | Ok (ValueKind RValue) ->
            "Cannot take address of temporary value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | Ok (ReferenceExpression { Declaration = { Register = Some _
                                                    Name = { Type = Identifier s } } }) ->
            sprintf "Cannot take address of register variable '%s'" s
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | _ ->
            expression
            |> Result.map (fun expr ->
                UnaryExpression
                    { Type = expr |> Expression.getType |> createPointer
                      Kind = AddressOf
                      Expression = expr
                      ValueKind = RValue })
    | Parse.DereferenceUnaryExpression (expression, token) ->
        let expression =
            visitUnaryExpression context expression
            |> Result.map lvalueConversion

        match expression with
        | Ok ((Type (PointerType ptrType)) as expr) ->
            UnaryExpression
                { Type = ptrType.ElementType
                  Kind = Dereference
                  Expression = expr
                  ValueKind = LValue }
            |> Ok
        | Ok _ ->
            "Cannot dereference non pointer value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | Error _ as s -> s
    | Parse.PlusUnaryExpression (expression, _) ->
        visitUnaryExpression context expression
        |> Result.map lvalueConversion
    | Parse.MinusUnaryExpression (expression, token) ->
        let expression =
            visitUnaryExpression context expression
            |> Result.map lvalueConversion

        match expression with
        | Ok (Type (PrimitiveType _)) -> expression
        | Ok _ ->
            "Cannot apply unary minus on non arithmetic value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | Error _ as s -> s
    | Parse.BitwiseNegateUnaryExpression (expression, token) ->
        let expression =
            visitUnaryExpression context expression
            |> Result.map lvalueConversion

        match expression with
        | Ok (Type (PrimitiveType (IntType _))) -> expression
        | Ok _ ->
            "Cannot apply bitwise negate on non integer value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | Error _ as s -> s
    | Parse.LogicalNegateUnaryExpression (expression, _) ->
        let expression =
            visitUnaryExpression context expression
            |> Result.map lvalueConversion

        match expression with
        | Ok expression ->
            UnaryExpression
                { Type = createInt ()
                  Kind = LogicalNegate
                  Expression = expression
                  ValueKind = RValue }
            |> Ok
        | Error _ as s -> s
    | Parse.SizeOfUnaryExpression _
    | Parse.SizeOfTypeUnaryExpression _ ->
        ConstantExpression { Type = createInt (); Value = 1s }
        |> Ok

and visitPostFixExpression (context: Context) (expression: Parse.PostFixExpression): Result<Expression, string> =
    match expression with
    | Parse.PrimaryPostFixExpression expression -> visitPrimaryExpression context expression
    | Parse.IncrementPostFixExpression (expression, token) ->
        let expression =
            visitPostFixExpression context expression

        match expression with
        | Ok (ValueKind RValue) ->
            "Cannot increment temporary value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | _ ->
            expression
            |> Result.map (fun expr ->
                UnaryExpression
                    { Type = Expression.getType expr
                      Kind = PostIncrement
                      Expression = expr
                      ValueKind = RValue })
    | Parse.DecrementPostFixExpression (expression, token) ->
        let expression =
            visitPostFixExpression context expression

        match expression with
        | Ok (ValueKind RValue) ->
            "Cannot decrement temporary value"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | _ ->
            expression
            |> Result.map (fun expr ->
                UnaryExpression
                    { Type = Expression.getType expr
                      Kind = PostDecrement
                      Expression = expr
                      ValueKind = RValue })
    | Parse.SubscriptPostFixExpression (lhs, openBracket, rhs) ->
        let lhs =
            visitPostFixExpression context lhs
            |> Result.map lvalueConversion

        let rhs =
            visitExpression context rhs
            |> Result.map lvalueConversion

        match (lhs, rhs) with
        | (Ok (Type (PrimitiveType _)), Ok (Type (PrimitiveType _)))
        | (Ok (Type (PointerType _)), Ok (Type (PointerType _))) ->
            "Expected one integer and one pointer type in subscript operator"
            |> context.SourceObject.emitError (ErrorTypeToken openBracket)
            |> Error
        | (Ok lhs, Ok rhs) ->
            let elementType =
                match (lhs, rhs) with
                | (Type (PointerType ptr), _) -> ptr.ElementType
                | (_, Type (PointerType ptr)) -> ptr.ElementType
                | _ -> failwith "Internal Compiler Error: One of the expressions is not a pointer type"

            BinaryExpression
                { Left = lhs
                  Right = rhs
                  Kind = SubScript
                  Type = elementType
                  ValueKind = LValue }
            |> Ok
        | (Error s1, Error s2) -> s1 + s2 |> Error
        | (Error s, Ok _)
        | (Ok _, Error s) -> Error s

and visitPrimaryExpression (context: Context) (expression: Parse.PrimaryExpression): Result<Expression, string> =
    match expression with
    | Parse.ExpressionPrimaryExpression expression -> visitExpression context expression
    | Parse.LiteralPrimaryExpression token ->
        ConstantExpression
            { Value = token |> Token.value
              Type = createInt () }
        |> Ok
    | Parse.IdentifierPrimaryExpression token ->
        let rec findIdentifier list =
            match list with
            | [] -> None
            | head :: tail ->
                match Map.tryFind (Token.identifier token) head with
                | None -> findIdentifier tail
                | Some _ as s -> s

        match context.Scopes |> findIdentifier with
        | None ->
            token
            |> Token.identifier
            |> sprintf "Unknown identifier '%s'"
            |> context.SourceObject.emitError (ErrorTypeToken token)
            |> Error
        | Some decl ->
            ReferenceExpression { Declaration = decl; Type = decl.Type }
            |> Ok


let rec visitStatement (context: Context) (statement: Parse.Statement) =
    match statement with
    | Parse.ExpressionStatement None -> (ExpressionStatement None |> Ok, context)
    | Parse.ExpressionStatement (Some expression) ->
        (visitExpression context expression
         |> Result.map (Some >> ExpressionStatement),
         context)
    | _ -> //TODO:
        (ExpressionStatement None |> Ok, context)

let private applyDeclarator aType (declarator: Parse.Declarator) =
    [ 0 .. (declarator.PointerCount - 1) ]
    |> List.fold (fun aType _ -> createPointer aType) aType

let visitDeclaration (context: Context) (declaration: Parse.Declaration) =
    let aType = createInt ()

    declaration.Declarators
    |> List.fold (fun (list, context) (declarator, maybeAssignment) ->
        let aType = applyDeclarator aType declarator

        let identifier = Token.identifier declarator.Identifier

        let element, context =
            match Map.tryFind identifier context.Scopes.Head with
            | Some _ ->
                (sprintf "Redeclaration of variable '%s'" identifier
                 |> context.SourceObject.emitError (ErrorTypeToken declarator.Identifier)
                 |> Error,
                 context)
            | None ->

                match maybeAssignment
                      |> Option.map (visitAssignmentExpression context) with
                | Some (Error s) -> (Error s, context)
                | Some (Ok expr) ->
                    let decl =
                        { Type = aType
                          Name = declarator.Identifier
                          Register = declaration.DeclarationSpecifiers
                          Initializer = Some expr }

                    let context =
                        { context with
                              Scopes =
                                  (Map.add identifier decl context.Scopes.Head)
                                  :: context.Scopes.Tail }

                    (decl |> Ok, context)
                | None ->
                    let decl =
                        { Type = aType
                          Name = declarator.Identifier
                          Register = declaration.DeclarationSpecifiers
                          Initializer = None }

                    let context =
                        { context with
                              Scopes =
                                  (Map.add identifier decl context.Scopes.Head)
                                  :: context.Scopes.Tail }

                    (decl |> Ok, context)

        (prependResult element list, context)) ([] |> Ok, context)
    |> fun (x, y) -> (x |> Result.map List.rev, y)

let visitCompoundItem (context: Context) (compoundItem: Parse.CompoundItem) =
    match compoundItem with
    | Parse.StatementCompoundItem statement ->
        let statement, context = visitStatement context statement
        (statement |> Result.map CompoundItemStatement, context)
    | Parse.DeclarationCompoundItem declaration ->
        let declaration, context = visitDeclaration context declaration
        (declaration |> Result.map CompoundItemDeclaration, context)

let visitTranslationUnit (context: Context) (translationUnit: Parse.CompoundItem list) =
    List.fold (fun (list, context) x ->
        let item, context = visitCompoundItem context x
        (prependResult item list, context)) ([] |> Ok, context) translationUnit
    |> fun (x, y) -> (x |> Result.map List.rev, y)

let analyse (sourceObject, translationUnit) =
    let context =
        { Scopes = [ Map([]) ]
          SourceObject = sourceObject }

    visitTranslationUnit context translationUnit
    |> fst
