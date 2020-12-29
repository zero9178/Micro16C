module Micro16C.Frontend.Sema

open Micro16C.Frontend.Lex
open Micro16C.Frontend.ErrorHandling

type Type =
    | PrimitiveType of Primitive
    | PointerType of Pointer

and Primitive = | IntType

and Pointer = { ElementType: Type }

let intType = IntType |> PrimitiveType

let createPointer elementType =
    { ElementType = elementType } |> PointerType

let rec typeToString aType =
    match aType with
    | PrimitiveType IntType -> "int"
    | PointerType { ElementType = elementType } -> typeToString elementType + "*"

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
    | ConditionalExpression of Conditional
    | AssignmentExpression of Assignment

and Binary =
    { Type: Type
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

and Conditional =
    { Type: Type
      Condition: Expression
      TrueBranch: Expression
      FalseBranch: Expression }

and AssignmentKind =
    | Normal
    | PlusAssign
    | MinusAssign
    | DivideAssign
    | MultiplyAssign
    | ModuloAssign
    | ShiftLeftAssign
    | ShiftRightAssign
    | BitAndAssign
    | BitOrAssign
    | BitXorAssign

and Assignment =
    { LValue: Expression
      Expression: Expression
      Kind: AssignmentKind }

and While =
    { Expression: Expression
      Statement: Statement }

and DoWhile =
    { Statement: Statement
      Expression: Expression }

and ForInitial =
    | ForInitialExpression of Expression option
    | ForInitialDeclaration of Declaration list

and For =
    { Initial: ForInitial
      Condition: Expression option
      Iteration: Expression option
      Statement: Statement }

and LoopStatement =
    | WhileLoop of While
    | DoWhileLoop of DoWhile
    | ForLoop of For

and Statement =
    | WhileStatement of While
    | DoWhileStatement of DoWhile
    | ForStatement of For
    | IfStatement of Expression * Statement * Statement option
    | BreakStatement of LoopStatement option ref
    | ContinueStatement of LoopStatement option ref
    | CompoundStatement of CompoundItem list
    | ExpressionStatement of Expression option
    | LabelStatement of string * Statement
    | GotoStatement of (string * Statement) option ref

and Declaration =
    { Type: Type
      Name: Token
      Register: Token option
      Initializer: Expression option }

and CompoundItem =
    | CompoundItemStatement of Statement
    | CompoundItemDeclaration of Declaration list

module Expression =
    let rec getType (expr: Expression) =
        match expr with
        | BinaryExpression expr -> expr.Type
        | UnaryExpression expr -> expr.Type
        | SizeofExpression expr -> expr.Type
        | CommaExpression expr -> expr.Type
        | ConstantExpression expr -> expr.Type
        | ReferenceExpression expr -> expr.Type
        | ConversionExpression expr -> expr.Type
        | ConditionalExpression expr -> expr.Type
        | AssignmentExpression expr -> getType expr.LValue

    let valueKind (expr: Expression) =
        match expr with
        | UnaryExpression { Kind = PreIncrement }
        | UnaryExpression { Kind = PreDecrement }
        | UnaryExpression { Kind = Dereference }
        | BinaryExpression { Kind = SubScript }
        | ReferenceExpression _ -> LValue
        | UnaryExpression _
        | BinaryExpression _
        | SizeofExpression _
        | CommaExpression _
        | ConstantExpression _
        | ConversionExpression _
        | ConditionalExpression _
        | AssignmentExpression _ -> RValue

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
      Loops: LoopStatement option ref list
      SourceObject: SourceObject
      Labels: Map<string, Statement>
      Gotos: (Token * (string * Statement) option ref) list }

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
    | list ->

        // Turn assignments from being stored as: expr (token * expr) (token * expr) into (expr * token) (expr * token) expr
        let lastExpr, list =
            list
            |> List.fold (fun (lastExpr, result) (token, expr) -> (expr, (lastExpr, token) :: result))
                   (expression.ConditionalExpression, [])

        let expression =
            visitConditionalExpression context lastExpr
            |> Result.map lvalueConversion

        list
        |> List.rev
        |> List.fold (fun rhs (lhs, token) ->
            let lhs = visitConditionalExpression context lhs

            let op =
                match token with
                | { Type = TokenType.Assignment } -> Normal
                | { Type = TokenType.PlusAssign } -> PlusAssign
                | { Type = TokenType.MinusAssign } -> MinusAssign
                | { Type = TokenType.DivideAssign } -> DivideAssign
                | { Type = TokenType.MultiplyAssign } -> MultiplyAssign
                | { Type = TokenType.ModuloAssign } -> ModuloAssign
                | { Type = TokenType.ShiftLeftAssign } -> ShiftLeftAssign
                | { Type = TokenType.ShiftRightAssign } -> ShiftRightAssign
                | { Type = TokenType.BitAndAssign } -> BitAndAssign
                | { Type = TokenType.BitOrAssign } -> BitOrAssign
                | { Type = TokenType.BitXorAssign } -> BitXorAssign
                | _ -> failwith "Internal Compiler Error: Invalid Token Type"

            match lhs with
            | (Ok (ValueKind RValue)) ->
                "Cannot assign to a temporary value"
                |> context.SourceObject.emitError (ErrorTypeToken token)
                |> Error
            | _ ->
                comb2 (fun lhs rhs ->
                    AssignmentExpression
                        { LValue = lhs
                          Expression = rhs
                          Kind = op }) lhs rhs) expression

and visitConditionalExpression (context: Context) (expression: Parse.ConditionalExpression): Result<Expression, string> =
    match expression.OptionalTernary with
    | None -> visitLogicalOrExpression context expression.LogicalOrExpression
    | Some (trueBranch, falseBranch) ->
        let condition =
            visitLogicalOrExpression context expression.LogicalOrExpression
            |> Result.map lvalueConversion

        let trueBranch =
            visitExpression context trueBranch
            |> Result.map lvalueConversion

        let falseBranch =
            visitConditionalExpression context falseBranch
            |> Result.map lvalueConversion

        comb3 (fun x y z ->
            let resultType =
                match (y, z) with
                | (_, Type (PointerType _ as resultType))
                | (Type resultType, _) -> resultType

            ConditionalExpression
                { Condition = x
                  TrueBranch = y
                  FalseBranch = z
                  Type = resultType }) condition trueBranch falseBranch

and visitLogicalOrExpression (context: Context) (expression: Parse.LogicalOrExpression): Result<Expression, string> =
    match expression.OptionalLogicalAndExpressions with
    | [] -> visitLogicalAndExpression context expression.LogicalAndExpression
    | list ->
        let expression =
            visitLogicalAndExpression context expression.LogicalAndExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs rhs ->
            let rhs =
                visitLogicalAndExpression context rhs
                |> Result.map lvalueConversion

            comb2 (fun lhs rhs ->
                BinaryExpression
                    { Type = intType
                      Left = lhs
                      Right = rhs
                      Kind = LogicOr }) lhs rhs) expression

and visitLogicalAndExpression (context: Context) (expression: Parse.LogicalAndExpression): Result<Expression, string> =
    match expression.OptionalInclusiveOrExpressions with
    | [] -> visitInclusiveOrExpression context expression.InclusiveOrExpression
    | list ->
        let expression =
            visitInclusiveOrExpression context expression.InclusiveOrExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs rhs ->
            let rhs =
                visitInclusiveOrExpression context rhs
                |> Result.map lvalueConversion

            comb2 (fun lhs rhs ->
                BinaryExpression
                    { Type = intType
                      Left = lhs
                      Right = rhs
                      Kind = LogicAnd }) lhs rhs) expression

and visitInclusiveOrExpression (context: Context) (expression: Parse.InclusiveOrExpression): Result<Expression, string> =
    match expression.OptionalExclusiveOrExpressions with
    | [] -> visitExclusiveOrExpression context expression.ExclusiveOrExpression
    | list ->
        let expression =
            visitExclusiveOrExpression context expression.ExclusiveOrExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitExclusiveOrExpression context rhs
                |> Result.map lvalueConversion

            match (lhs, rhs) with
            | (Error _, _)
            | (_, Error _)
            | (Ok (Type (PrimitiveType _)), Ok (Type (PrimitiveType _))) ->
                comb2 (fun lhs rhs ->
                    BinaryExpression
                        { Type = Expression.getType lhs
                          Left = lhs
                          Right = rhs
                          Kind = BitOr }) lhs rhs
            | (Ok _, Ok _) ->
                "Bit-or only supported on integer types"
                |> context.SourceObject.emitError (ErrorTypeToken token)
                |> Error) expression

and visitExclusiveOrExpression (context: Context) (expression: Parse.ExclusiveOrExpression): Result<Expression, string> =
    match expression.OptionalAndExpressions with
    | [] -> visitAndExpression context expression.AndExpression
    | list ->
        let expression =
            visitAndExpression context expression.AndExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitAndExpression context rhs
                |> Result.map lvalueConversion

            match (lhs, rhs) with
            | (Error _, _)
            | (_, Error _)
            | (Ok (Type (PrimitiveType _)), Ok (Type (PrimitiveType _))) ->
                comb2 (fun lhs rhs ->
                    BinaryExpression
                        { Type = Expression.getType lhs
                          Left = lhs
                          Right = rhs
                          Kind = BitXor }) lhs rhs
            | (Ok _, Ok _) ->
                "Bit-xor only supported on integer types"
                |> context.SourceObject.emitError (ErrorTypeToken token)
                |> Error) expression

and visitAndExpression (context: Context) (expression: Parse.AndExpression): Result<Expression, string> =
    match expression.OptionalEqualityExpressions with
    | [] -> visitEqualityExpression context expression.EqualityExpression
    | list ->
        let expression =
            visitEqualityExpression context expression.EqualityExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitEqualityExpression context rhs
                |> Result.map lvalueConversion

            match (lhs, rhs) with
            | (Error _, _)
            | (_, Error _)
            | (Ok (Type (PrimitiveType _)), Ok (Type (PrimitiveType _))) ->
                comb2 (fun lhs rhs ->
                    BinaryExpression
                        { Type = Expression.getType lhs
                          Left = lhs
                          Right = rhs
                          Kind = BitAnd }) lhs rhs
            | (Ok _, Ok _) ->
                "Bit-and only supported on integer types"
                |> context.SourceObject.emitError (ErrorTypeToken token)
                |> Error) expression

and visitEqualityExpression (context: Context) (expression: Parse.EqualityExpression): Result<Expression, string> =
    match expression.OptionalRelationalExpressions with
    | [] -> visitRelationalExpression context expression.RelationalExpression
    | list ->
        let expression =
            visitRelationalExpression context expression.RelationalExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitRelationalExpression context rhs
                |> Result.map lvalueConversion

            let op =
                match token with
                | { Type = TokenType.Equal } -> NotEqual
                | { Type = TokenType.NotEqual } -> Equal
                | _ -> failwith "Internal Compiler Error: Invalid Token Type"

            comb2 (fun lhs rhs ->
                BinaryExpression
                    { Type = intType
                      Left = lhs
                      Right = rhs
                      Kind = op }) lhs rhs) expression

and visitRelationalExpression (context: Context) (expression: Parse.RelationalExpression): Result<Expression, string> =
    match expression.OptionalShiftExpressions with
    | [] -> visitShiftExpression context expression.ShiftExpression
    | list ->
        let expression =
            visitShiftExpression context expression.ShiftExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitShiftExpression context rhs
                |> Result.map lvalueConversion

            let op =
                match token with
                | { Type = TokenType.GreaterThan } -> GreaterThan
                | { Type = TokenType.LessThan } -> LessThan
                | { Type = TokenType.GreaterThanOrEqual } -> GreaterThanOrEqual
                | { Type = TokenType.LessThanOrEqual } -> LessThanOrEqual
                | _ -> failwith "Internal Compiler Error: Invalid Token Type"

            comb2 (fun lhs rhs ->
                BinaryExpression
                    { Type = intType
                      Left = lhs
                      Right = rhs
                      Kind = op }) lhs rhs) expression

and visitShiftExpression (context: Context) (expression: Parse.ShiftExpression): Result<Expression, string> =
    match expression.OptionalAdditiveExpressions with
    | [] -> visitAdditiveExpression context expression.AdditiveExpression
    | list ->
        let expression =
            visitAdditiveExpression context expression.AdditiveExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitAdditiveExpression context rhs
                |> Result.map lvalueConversion

            let op =
                match token with
                | { Type = TokenType.ShiftLeft } -> ShiftLeft
                | { Type = TokenType.ShiftRight } -> ShiftRight
                | _ -> failwith "Internal Compiler Error: Invalid Token Type"

            match (lhs, rhs) with
            | (Error _, _)
            | (_, Error _)
            | (Ok (Type (PrimitiveType _)), Ok (Type (PrimitiveType _))) ->
                comb2 (fun lhs rhs ->

                    BinaryExpression
                        { Type = Expression.getType lhs
                          Left = lhs
                          Right = rhs
                          Kind = op }) lhs rhs
            | (Ok _, Ok _) ->
                let opName =
                    match op with
                    | ShiftLeft -> "Left shift"
                    | ShiftRight -> "Right shift"
                    | _ -> ""

                sprintf "%s only supported on integer types" opName
                |> context.SourceObject.emitError (ErrorTypeToken token)
                |> Error) expression

and visitAdditiveExpression (context: Context) (expression: Parse.AdditiveExpression): Result<Expression, string> =
    match expression.OptionalMultiplicativeExpressions with
    | [] -> visitMultiplicativeExpression context expression.MultiplicativeExpression
    | list ->
        let expression =
            visitMultiplicativeExpression context expression.MultiplicativeExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitMultiplicativeExpression context rhs
                |> Result.map lvalueConversion

            let op =
                match token with
                | { Type = TokenType.Plus } -> Plus
                | { Type = TokenType.Minus } -> BinaryOperator.Minus
                | _ -> failwith "Internal Compiler Error: Invalid Token Type"

            match (lhs, rhs, op) with
            | (Error s, Ok _, _)
            | (Ok _, Error s, _) -> s |> Error
            | (Error s1, Error s2, _) -> s1 + s2 |> Error
            | (Ok (Type ((PointerType _) as resultType) as lhs), Ok (Type (PrimitiveType IntType) as rhs), _)
            | (Ok (Type ((PointerType _) as resultType) as lhs), Ok (Type (PointerType _) as rhs), BinaryOperator.Minus)
            | (Ok (Type (PrimitiveType IntType) as lhs), Ok (Type ((PointerType _) as resultType) as rhs), Plus)
            | (Ok (Type ((PrimitiveType _) as resultType) as lhs), Ok (Type (PrimitiveType _) as rhs), _) ->
                let resultType =
                    match (lhs, rhs) with
                    | (Type (PointerType _), Type (PointerType _)) -> intType
                    | _ -> resultType

                BinaryExpression
                    { Type = resultType
                      Left = lhs
                      Right = rhs
                      Kind = op }
                |> Ok
            | (Ok lhs, Ok rhs, _) ->
                let opName =
                    match op with
                    | Plus -> "Addition"
                    | BinaryOperator.Minus -> "Subtraction"
                    | _ -> ""

                sprintf
                    "%s not allowed on operators of type %s and %s"
                    opName
                    (lhs |> Expression.getType |> typeToString)
                    (rhs |> Expression.getType |> typeToString)
                |> context.SourceObject.emitError (ErrorTypeToken token)
                |> Error) expression

and visitMultiplicativeExpression (context: Context)
                                  (expression: Parse.MultiplicativeExpression)
                                  : Result<Expression, string> =
    match expression.OptionalUnaryExpressions with
    | [] -> visitUnaryExpression context expression.UnaryExpression
    | list ->
        let expression =
            visitUnaryExpression context expression.UnaryExpression
            |> Result.map lvalueConversion

        list
        |> List.fold (fun lhs (token, rhs) ->
            let rhs =
                visitUnaryExpression context rhs
                |> Result.map lvalueConversion

            let op =
                match token with
                | { Type = TokenType.Division } -> Division
                | { Type = Asterisk } -> Multiply
                | { Type = Percent } -> Modulo
                | _ -> failwith "Internal Compiler Error: Invalid Token Type"

            match (lhs, rhs) with
            | (Error _, _)
            | (_, Error _)
            | (Ok (Type (PrimitiveType _)), Ok (Type (PrimitiveType _))) ->
                comb2 (fun lhs rhs ->

                    BinaryExpression
                        { Type = Expression.getType lhs
                          Left = lhs
                          Right = rhs
                          Kind = op }) lhs rhs
            | (Ok _, Ok _) ->
                let opName, typeName =
                    match op with
                    | Division -> "Division", "arithmetic"
                    | Multiply -> "Multiply", "arithmetic"
                    | Modulo -> "Modulo", "integer"
                    | _ -> "", ""

                sprintf "%s only supported on %s types" opName typeName
                |> context.SourceObject.emitError (ErrorTypeToken token)
                |> Error) expression

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
                { Type = intType
                  Kind = LogicalNegate
                  Expression = expression
                  ValueKind = RValue }
            |> Ok
        | Error _ as s -> s
    | Parse.SizeOfUnaryExpression _
    | Parse.SizeOfTypeUnaryExpression _ ->
        ConstantExpression { Type = intType; Value = 1s }
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
                  Type = elementType }
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
              Type = intType }
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

let private applyDeclarator aType (declarator: Parse.Declarator) =
    [ 0 .. (declarator.PointerCount - 1) ]
    |> List.fold (fun aType _ -> createPointer aType) aType

let rec visitStatement (context: Context) (statement: Parse.Statement) =
    match statement with
    | Parse.ExpressionStatement None -> (ExpressionStatement None |> Ok, context)
    | Parse.ExpressionStatement (Some expression) ->
        (visitExpression context expression
         |> Result.map (Some >> ExpressionStatement),
         context)
    | Parse.WhileStatement (expression, statement) ->
        let whileLoop = ref None

        let oldContext = context

        let context =
            { context with
                  Loops = whileLoop :: context.Loops }

        let condition = visitExpression context expression
        let statement, context = visitStatement context statement

        (comb2 (fun x y ->
            let whileObj = { While.Expression = x; Statement = y }
            whileLoop := WhileLoop whileObj |> Some
            WhileStatement whileObj) condition statement,
         { oldContext with
               Labels = context.Labels
               Gotos = context.Gotos })
    | Parse.DoWhileStatement (statement, expression) ->
        let doWhileLoop = ref None

        let oldContext = context

        let context =
            { context with
                  Loops = doWhileLoop :: context.Loops }

        let statement, context = visitStatement context statement
        let condition = visitExpression context expression

        (comb2 (fun x y ->
            let doWhileObj =
                { DoWhile.Expression = x
                  Statement = y }

            doWhileLoop := DoWhileLoop doWhileObj |> Some
            DoWhileStatement doWhileObj) condition statement,
         { oldContext with
               Labels = context.Labels
               Gotos = context.Gotos })
    | Parse.IfStatement (expression, statement, elseBranch) ->
        let expression = visitExpression context expression
        let statement, context = visitStatement context statement

        let elseBranch, context =
            match elseBranch with
            | None -> (None, context)
            | Some elseBranch ->
                let elseBranch, context = visitStatement context elseBranch
                (Some elseBranch, context)

        match elseBranch with
        | None -> (comb2 (fun x y -> IfStatement(x, y, None)) expression statement, context)
        | Some elseBranch -> (comb3 (fun x y z -> IfStatement(x, y, Some z)) expression statement elseBranch, context)
    | Parse.CompoundStatement list ->
        let oldContext = context

        let context =
            { context with
                  Scopes = [ Map([]) ] @ context.Scopes }

        let items, context = visitCompoundStatement context list

        (items |> Result.map CompoundStatement,
         { oldContext with
               Labels = context.Labels
               Gotos = context.Gotos })
    | Parse.ContinueStatement token ->
        match context.Loops with
        | [] ->
            ("Cannot use 'continue' outside of a loop"
             |> context.SourceObject.emitError (ErrorTypeToken token)
             |> Error,
             context)
        | loop :: _ -> (ContinueStatement loop |> Ok, context)
    | Parse.BreakStatement token ->
        match context.Loops with
        | [] ->
            ("Cannot use 'break' outside of a loop"
             |> context.SourceObject.emitError (ErrorTypeToken token)
             |> Error,
             context)
        | loop :: _ -> (BreakStatement loop |> Ok, context)
    | Parse.ForStatementDecl (_, condition, iteration, statement)
    | Parse.ForStatement (_, condition, iteration, statement) ->

        let oRtoRo optionResult =
            match optionResult with
            | None -> None |> Ok
            | Some (Ok value) -> Some value |> Ok
            | Some (Error s) -> Error s

        let oldContext = context

        let initial, context =
            match statement with
            | Parse.ForStatementDecl (initial, _, _, _) ->
                let context =
                    { context with
                          Scopes = Map([]) :: context.Scopes }

                let initial, context = visitDeclaration context initial
                (initial |> Result.map ForInitialDeclaration, context)
            | Parse.ForStatement (initial, _, _, _) ->
                (initial
                 |> Option.map (visitExpression context)
                 |> oRtoRo
                 |> Result.map ForInitialExpression,
                 context)
            | _ -> failwith "Internal Compiler Error: Expected only for loop variants"

        let condition =
            condition
            |> Option.map (visitExpression context)
            |> oRtoRo

        let iteration =
            iteration
            |> Option.map (visitExpression context)
            |> oRtoRo

        let forLoop = ref None

        let context =
            { context with
                  Loops = forLoop :: context.Loops }

        let statement, context = visitStatement context statement

        (comb4 (fun x y z w ->
            let forObj =
                { Initial = x
                  Condition = y
                  Iteration = z
                  Statement = w }

            forLoop := ForLoop forObj |> Some
            ForStatement forObj) initial condition iteration statement,
         { oldContext with
               Labels = context.Labels
               Gotos = context.Gotos })
    | Parse.LabelStatement (name, statement) ->
        let statement, context = visitStatement context statement

        let statement =
            statement
            |> Result.map (pack2 (Token.identifier name) >> LabelStatement)

        match statement with
        | Ok label ->
            match Map.tryFind (Token.identifier name) context.Labels with
            | Some _ ->
                (sprintf "Duplicate label '%s'"
                 <| Token.identifier name
                 |> context.SourceObject.emitError (ErrorTypeToken name)
                 |> Error,
                 context)
            | None ->
                (statement,
                 { context with
                       Labels = Map.add (Token.identifier name) label context.Labels })
        | _ -> (statement, context)
    | Parse.GotoStatement name ->
        match Map.tryFind (Token.identifier name) context.Labels with
        | Some (LabelStatement (name, statement)) ->
            ((name, statement)
             |> Some
             |> ref
             |> GotoStatement
             |> Ok,
             context)
        | Some _ -> failwith "Internal Compiler Error: Statement is not a Label"
        | None ->
            let noneRef = ref None

            (noneRef |> GotoStatement |> Ok,
             { context with
                   Gotos = (name, noneRef) :: context.Gotos })


and visitDeclaration (context: Context) (declaration: Parse.Declaration) =
    let aType = intType

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
                    let expr = lvalueConversion expr

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

and visitCompoundItem (context: Context) (compoundItem: Parse.CompoundItem) =
    match compoundItem with
    | Parse.StatementCompoundItem statement ->
        let statement, context = visitStatement context statement
        (statement |> Result.map CompoundItemStatement, context)
    | Parse.DeclarationCompoundItem declaration ->
        let declaration, context = visitDeclaration context declaration
        (declaration |> Result.map CompoundItemDeclaration, context)

and visitCompoundStatement (context: Context) (translationUnit: Parse.CompoundItem list) =
    List.fold (fun (list, context) x ->
        let item, context = visitCompoundItem context x
        (prependResult item list, context)) ([] |> Ok, context) translationUnit
    |> fun (x, y) -> (x |> Result.map List.rev, y)

let analyse (sourceObject, translationUnit) =
    let context =
        { Scopes = [ Map([]) ]
          SourceObject = sourceObject
          Loops = []
          Labels = Map([])
          Gotos = [] }

    let items, context =
        visitCompoundStatement context translationUnit

    let error =
        context.Gotos
        |> List.fold (fun result (token, ref) ->
            match Map.tryFind (Token.identifier token) context.Labels with
            | Some (LabelStatement (name, statement)) ->
                ref := Some(name, statement)
                result
            | Some _ -> failwith "Internal Compiler Error: Statement is not a Label"
            | None ->
                let error =
                    sprintf "Unresolved label '%s'" (Token.identifier token)
                    |> context.SourceObject.emitError (ErrorTypeToken token)
                    |> Error

                comb2 (fun _ _ -> ()) result error) (() |> Ok)

    comb2 (fun _ -> id) error items
