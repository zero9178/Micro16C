module Micro16C.Frontend.Sema


type Type =
    | PrimitiveType
    | PointerType

and PrimitiveType = | IntType

and PointerType = { ElementType: Type }

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
    | Plus
    | Minus
    | BitwiseNegate
    | LogicalNegate

type Expression =
    | BinaryExpression
    | UnaryExpression
    | SizeofExpression

and BinaryExpression =
    { Type: Type
      Left: Expression
      Right: Expression
      Kind: BinaryOperator }

and UnaryExpression =
    { Type: Type
      Expression: Expression
      Kind: UnaryOperator }

and SizeofExpression = { Type: Type; Size: int }

let visitExpression (expression: Parse.Expression) = ()

let visitAssignmentExpression (expression: Parse.AssignmentExpression) = ()

let visitConditionalExpression (expression: Parse.ConditionalExpression) = ()

let visitLogicalOrExpression (expression: Parse.LogicalOrExpression) = ()

let visitLogicalAndExpression (expression: Parse.LogicalAndExpression) = ()

let visitInclusiveOrExpression (expression: Parse.InclusiveOrExpression) = ()

let visitExclusiveOrExpression (expression: Parse.ExclusiveOrExpression) = ()

let visitAndExpression (expression: Parse.AndExpression) = ()

let visitEqualityExpression (expression: Parse.EqualityExpression) = ()

let visitRelationalExpression (expression: Parse.RelationalExpression) = ()

let visitShiftExpression (expression: Parse.ShiftExpression) = ()

let visitAdditiveExpression (expression: Parse.AdditiveExpression) = ()

let visitMultiplicativeExpression (expression: Parse.MultiplicativeExpression) = ()

let visitUnaryExpression (expression: Parse.UnaryExpression) = ()

let visitPostFixExpression (expression: Parse.PostFixExpression) = ()

let visitPrimaryExpression (expression: Parse.PrimaryExpression) = ()
