module Micro16C.Frontend.Parse

open System
open System.Data
open System.Linq.Expressions
open Micro16C.Frontend.Lex

(*
<TranslationUnit> ::= { <Statements> | <Declaration>}

<Declaration> ::= <DeclarationSpecifiers> <Declarator> [ '=' <AssignmentExpression> ] { ',' <Declarator> [ '=' <AssignmentExpression> ] } ';'

<DeclarationSpecifiers> ::= 'int'

<Declarator> ::= {'*'} IDENTIFIER

<AbstractDeclarator> ::= {'*'}

<TypeName> ::= <DeclarationSpecifiers> [<AbstractDeclarator>]

<Expression> ::= <AssignmentExpression> {',' <AssignmentExpression>}

<AssignmentExpression> ::= <ConditionalExpression> {<AssignmentOperator> <ConditionalExpression>}

<AssignmentOperator> ::= '='
                       | '*='
                       | '/='
                       | '%='
                       | '+='
                       | '-='
                       | '<<='
                       | '>>='
                       | '&='
                       | '^='
                       | '|='

<ConditionalExpression> ::= <LogicalOrExpression> [ '?' <Expression> ':' <ConditionalExpression> ]

<LogicalOrExpression> ::= <LogicalAndExpression> { '||' <LogicalAndExpression> }

<LogicalAndExpression> ::= <InclusiveOrExpression> { '&&' <InclusiveOrExpression> }

<InclusiveOrExpression> ::= <ExclusiveOrExpression> { '|' <ExclusiveOrExpression> }

<ExclusiveOrExpression> ::= <AndExpression> { '^' <AndExpression> }

<AndExpression> ::= <EqualityExpression> { '&' <EqualityExpression> }

<EqualityExpression> ::= <RelationalExpression> { ('==' | '!=') <RelationalExpression> }

<RelationalExpression> ::= <ShiftExpression> { ('<' | '>' | '<=' | '>=') <ShiftExpression> }

<ShiftExpression> ::= <AdditiveExpression> { ('<<' | '>>') <AdditiveExpression> }

<AdditiveExpression> ::= <MultiplicativeExpression> { ('+' | '-') <MultiplicativeExpression> }

<MultiplicativeExpression> ::= <UnaryExpression> { ('*' | '/' | '%') <UnaryExpression> }

<UnaryExpression> ::= <PostFixExpression>
                    | '++' <UnaryExpression>
                    | '--' <UnaryExpression>
                    | '&' <UnaryExpression>
                    | '*' <UnaryExpression>
                    | '+' <UnaryExpression>
                    | '-' <UnaryExpression>
                    | '~' <UnaryExpression>
                    | '!' <UnaryExpression>
                    | 'sizeof' <UnaryExpression>
                    | 'sizeof' '(' <TypeName> ')'

<PostFixExpression> ::= <PrimaryExpression>
                      | <PostFixExpression> [ <Expression> ]
                      | <PostFixExpression> '++'
                      | <PostFixExpression> '--'

<PrimaryExpression> ::= IDENTIFIER
                      | LITERAL
                      | '(' <Expression> ')'

<Statement> ::= <IfStatement>
               | <WhileStatement>
               | <DoWhileStatement>
               | <ForStatement>
               | <BreakStatement>
               | <ContinueStatement>
               | <ExpressionStatement>
               | <GotoStatement>
               | <LabelStatement>
               | <CompoundStatement>

<IfStatement> ::= 'if' '(' <Expression> ')' <Statement> [ 'else' <Statement> ]

<WhileStatement> ::= 'while' '(' <Expression> ')' <Statement>

<DoWhileStatement> ::= 'do' <Statement> 'while' '(' <Expression> ')' ';'

<ForStatement> ::= 'for' '(' ([<Expression>] ';' | <Declaration>) [<Expression>] ';' [<Expression>] ')' <Statement>

<BreakStatement> ::= 'break' ';'

<ContinueStatement> ::= 'continue' ';'

<GotoStatement> ::= 'goto' IDENTIFIER ';'

<LabelStatement> ::= IDENTIFIER ':' <Statement>

<CompoundStatement> ::= '{' (<Statement> | <Declaration>) '}'

*)


type Declarator =
    { PointerCount: int
      Identifier: Token option }

type TypeName = { PointerCount: int }

type Expression =
    { AssignmentExpression: AssignmentExpression
      OptionalAssignmentExpressions: AssignmentExpression list }

and AssignmentExpression =
    { ConditionalExpression: ConditionalExpression
      OptionalConditionalExpressions: (Token * ConditionalExpression) list }

and ConditionalExpression =
    { LogicalOrExpression: LogicalOrExpression
      OptionalTernary: (Expression * ConditionalExpression) option }

and LogicalOrExpression =
    { LogicalAndExpression: LogicalAndExpression
      OptionalLogicalAndExpressions: LogicalAndExpression list }

and LogicalAndExpression =
    { InclusiveOrExpression: InclusiveOrExpression
      OptionalInclusiveOrExpressions: InclusiveOrExpression list }

and InclusiveOrExpression =
    { ExclusiveOrExpression: ExclusiveOrExpression
      OptionalExclusiveOrExpressions: ExclusiveOrExpression list }

and ExclusiveOrExpression =
    { AndExpression: AndExpression
      OptionalAndExpressions: AndExpression list }

and AndExpression =
    { EqualityExpression: EqualityExpression
      OptionalEqualityExpressions: EqualityExpression list }

and EqualityExpression =
    { RelationalExpression: RelationalExpression
      OptionalRelationalExpressions: (Token * RelationalExpression) list }

and RelationalExpression =
    { ShiftExpression: ShiftExpression
      OptionalShiftExpressions: (Token * ShiftExpression) list }

and ShiftExpression =
    { AdditiveExpression: AdditiveExpression
      OptionalAdditiveExpressions: (Token * AdditiveExpression) list }

and AdditiveExpression =
    { MultiplicativeExpression: MultiplicativeExpression
      OptionalMultiplicativeExpressions: (Token * MultiplicativeExpression) list }

and MultiplicativeExpression =
    { UnaryExpression: UnaryExpression
      OptionalUnaryExpressions: (Token * UnaryExpression) list }

and UnaryExpression =
    | PostFixUnaryExpression of PostFixExpression
    | IncrementUnaryExpression of UnaryExpression
    | DecrementUnaryExpression of UnaryExpression
    | AddressOfUnaryExpression of UnaryExpression
    | DereferenceUnaryExpression of UnaryExpression
    | PlusUnaryExpression of UnaryExpression
    | MinusUnaryExpression of UnaryExpression
    | BitwiseNegateUnaryExpression of UnaryExpression
    | LogicalNegateUnaryExpression of UnaryExpression
    | SizeOfUnaryExpression of UnaryExpression
    | SizeOfTypeUnaryExpression of TypeName

and PostFixExpression =
    | PrimaryPostFixExpression of PrimaryExpression
    | SubscriptPostFixExpression of PostFixExpression * Expression
    | IncrementPostFixExpression of PostFixExpression
    | DecrementPostFixExpression of PostFixExpression

and PrimaryExpression =
    | IdentifierPrimaryExpression of Token
    | LiteralPrimaryExpression of Token
    | ExpressionPrimaryExpression of Expression option

type Declaration =
    { Declarators: (Declarator * AssignmentExpression option) list }

type CompoundItem =
    | StatementCompoundItem of Statement
    | DeclarationCompoundItem of Declaration

and Statement =
    | WhileStatement of Expression * Statement
    | DoWhileStatement of Statement * Expression
    | ForStatementDecl of Declaration * Expression option * Expression option
    | ForStatement of Expression option * Expression option * Expression option
    | BreakStatement
    | ContinueStatement
    | GotoStatement of string
    | LabelStatement of string * Statement
    | CompoundStatement of CompoundItem list

let private parseBinaryOperator subExprParse allowedType error (tokens: Token list) =
    let lhs, tokens = subExprParse error tokens

    let rec parseOptionalRhs (tokens: Token list) =
        match tokens with
        | { Type = t } :: tokens when t = allowedType ->
            let rhs, tokens = subExprParse error tokens

            let others, tokens = parseOptionalRhs tokens

            (rhs :: others, tokens)
        | _ -> ([], tokens)

    let optionalRhs, tokens = parseOptionalRhs tokens

    (lhs, optionalRhs, tokens)

let private expect tokenType error (tokens: Token list) message =
    match List.tryHead tokens with
    | Some { Type = t } when t = tokenType -> (Some(List.head tokens), List.tail tokens)
    | Some t ->
        message |> error (ErrorTypeToken t)
        (None, tokens)
    | None ->
        message |> error ErrorTypeEnd
        (None, tokens)

let private parseBinaryMultiOperator subExprParse allowedTypes error (tokens: Token list) =
    let lhs, tokens = subExprParse error tokens

    let rec parseOptionalRhs (tokens: Token list) =
        match tokens with
        | { Type = t } :: tokens as ts when List.contains t allowedTypes ->
            let rhs, tokens = subExprParse error tokens

            let others, tokens = parseOptionalRhs tokens

            ((List.head ts, rhs) :: others, tokens)
        | _ -> ([], tokens)

    let optionalRhs, tokens = parseOptionalRhs tokens

    (lhs, optionalRhs, tokens)

let rec parseExpression error (tokens: Token list) =

    let lhs, rhs, tokens =
        parseBinaryOperator parseAssignmentExpression Comma error tokens

    ({ AssignmentExpression = lhs
       OptionalAssignmentExpressions = rhs },
     tokens)

and parseAssignmentExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator
            parseConditionalExpression
            [ Assignment
              MultiplyAssign
              DivideAssign
              ModuloAssign
              PlusAssign
              MinusAssign
              ShiftLeftAssign
              ShiftRightAssign
              BitAndAssign
              BitXorAssign
              BitOrAssign ]
            error
            tokens

    ({ ConditionalExpression = lhs
       OptionalConditionalExpressions = rhs },
     tokens)

and parseConditionalExpression error (tokens: Token list) =
    let logicalOrExpression, tokens = parseLogicalOrExpression error tokens

    match tokens with
    | { Type = QuestionMark } :: tokens ->
        let expression, tokens = parseExpression error tokens

        match expect Colon error tokens "Expected ':' to match '?'" with
        | (Some _, tokens) ->
            let conditionalExpression, tokens = parseConditionalExpression error tokens

            ({ LogicalOrExpression = logicalOrExpression
               OptionalTernary = Some(expression, conditionalExpression) },
             tokens)
        | (_, tokens) ->
            ({ LogicalOrExpression = logicalOrExpression
               OptionalTernary = None },
             tokens)
    | _ ->
        ({ LogicalOrExpression = logicalOrExpression
           OptionalTernary = None },
         tokens)

and parseLogicalOrExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryOperator parseLogicalAndExpression LogicOr error tokens

    ({ LogicalAndExpression = lhs
       OptionalLogicalAndExpressions = rhs },
     tokens)

and parseLogicalAndExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryOperator parseInclusiveOrExpression LogicAnd error tokens

    ({ InclusiveOrExpression = lhs
       OptionalInclusiveOrExpressions = rhs },
     tokens)

and parseInclusiveOrExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryOperator parseExclusiveOrExpression BitOr error tokens

    ({ ExclusiveOrExpression = lhs
       OptionalExclusiveOrExpressions = rhs },
     tokens)

and parseExclusiveOrExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryOperator parseAndExpression BitXor error tokens

    ({ AndExpression = lhs
       OptionalAndExpressions = rhs },
     tokens)

and parseAndExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryOperator parseEqualityExpression Ampersand error tokens

    ({ EqualityExpression = lhs
       OptionalEqualityExpressions = rhs },
     tokens)

and parseEqualityExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseRelationalExpression [ Equal; NotEqual ] error tokens

    ({ RelationalExpression = lhs
       OptionalRelationalExpressions = rhs },
     tokens)

and parseRelationalExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator
            parseShiftExpression
            [ LessThan
              GreaterThan
              LessThanOrEqual
              GreaterThanOrEqual ]
            error
            tokens

    ({ ShiftExpression = lhs
       OptionalShiftExpressions = rhs },
     tokens)

and parseShiftExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseAdditiveExpression [ ShiftRight; ShiftLeft ] error tokens

    ({ AdditiveExpression = lhs
       OptionalAdditiveExpressions = rhs },
     tokens)

and parseAdditiveExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseMultiplicativeExpression [ Plus; Minus ] error tokens

    ({ MultiplicativeExpression = lhs
       OptionalMultiplicativeExpressions = rhs },
     tokens)

and parseMultiplicativeExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseUnaryExpression [ Asterisk; Division; Percent ] error tokens

    ({ UnaryExpression = lhs
       OptionalUnaryExpressions = rhs },
     tokens)

and parseUnaryExpression error (tokens: Token list) =
    match tokens with
    | { Type = Increment } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (IncrementUnaryExpression unary, tokens)
    | { Type = Decrement } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (DecrementUnaryExpression unary, tokens)
    | { Type = Ampersand } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (AddressOfUnaryExpression unary, tokens)
    | { Type = Asterisk } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (DereferenceUnaryExpression unary, tokens)
    | { Type = Plus } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (PlusUnaryExpression unary, tokens)
    | { Type = Minus } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (MinusUnaryExpression unary, tokens)
    | { Type = BitWiseNegation } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (BitwiseNegateUnaryExpression unary, tokens)
    | { Type = LogicalNegation } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (LogicalNegateUnaryExpression unary, tokens)
    | { Type = SizeOfKeyword } :: { Type = OpenParentheses } :: { Type = IntKeyword } :: tokens ->
        let pointerCount =
            tokens
            |> List.takeWhile (function
                | { Type = Asterisk } -> true
                | _ -> false)
            |> List.length

        let tokens = tokens |> List.skip pointerCount
        (SizeOfTypeUnaryExpression { PointerCount = pointerCount }, tokens)
    | { Type = SizeOfKeyword } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (SizeOfUnaryExpression unary, tokens)
    | _ ->
        let postFix, tokens = parsePostFixExpression error tokens
        (PostFixUnaryExpression postFix, tokens)

and parsePostFixExpression error (tokens: Token list) =
    let primary, tokens =
        match tokens with
        | { Type = Identifier _ } :: tokens as t -> (t |> List.head |> IdentifierPrimaryExpression, tokens)
        | { Type = Literal _ } :: tokens as t -> (t |> List.head |> LiteralPrimaryExpression, tokens)
        | { Type = OpenParentheses } :: tokens ->
            let expression, tokens = parseExpression error tokens

            let _, tokens =
                expect CloseParentheses error tokens "Expected ')' to match '('"

            (ExpressionPrimaryExpression(Some expression), tokens)
        | [] ->
            "Expected Identifier, Literal or '('"
            |> error ErrorTypeEnd

            (ExpressionPrimaryExpression None, tokens)
        | t :: _ ->
            "Expected Identifier, Literal or '('"
            |> error (ErrorTypeToken t)

            (ExpressionPrimaryExpression None, tokens)

    let rec parsePostFixExpressionSuffix prev (tokens: Token list) =

        match tokens with
        | { Type = OpenSquareBracket } :: tokens ->
            let expression, tokens = parseExpression error tokens

            let _, tokens =
                expect CloseSquareBracket error tokens "Expected ']' to match '['"

            parsePostFixExpressionSuffix (SubscriptPostFixExpression(prev, expression)) tokens
        | { Type = Increment } :: tokens -> parsePostFixExpressionSuffix (IncrementPostFixExpression prev) tokens
        | { Type = Decrement } :: tokens -> parsePostFixExpressionSuffix (DecrementPostFixExpression prev) tokens
        | _ -> (prev, tokens)

    parsePostFixExpressionSuffix (PrimaryPostFixExpression primary) tokens

let parseDeclaration error (tokens: Token list) =
    assert match List.head tokens with
           | { Type = IntKeyword } -> true
           | _ -> false

    let tokens = tokens |> List.tail

    let parseDeclarator tokens =

        let pointerCount =
            tokens
            |> List.takeWhile (function
                | { Type = Asterisk } -> true
                | _ -> false)
            |> List.length

        let tokens = tokens |> List.skip pointerCount

        match List.tryHead tokens with
        | Some { Type = Identifier _ } as t ->
            ({ PointerCount = pointerCount
               Identifier = t },
             List.tail tokens)
        | Some t ->
            "Expected identifier in declaration"
            |> error (ErrorTypeToken t)

            ({ PointerCount = pointerCount
               Identifier = Some t },
             List.tail tokens)
        | None as t ->
            "Expected identifier in declaration"
            |> error ErrorTypeEnd

            ({ PointerCount = pointerCount
               Identifier = t },
             [])

    let firstDeclarator, tokens = parseDeclarator tokens

    let assignment, tokens =
        match tokens with
        | { Type = Assignment } :: tokens ->
            let assignment, tokens = parseAssignmentExpression error tokens
            (Some assignment, tokens)
        | _ -> (None, tokens)

    let rec parseDeclarators error (tokens: Token list) =
        match tokens with
        | { Type = Comma } :: tokens ->
            let declarator, tokens = parseDeclarator tokens

            let assignment, tokens =
                match tokens with
                | { Type = Assignment } :: tokens ->
                    let assignment, tokens = parseAssignmentExpression error tokens
                    (Some assignment, tokens)
                | _ -> (None, tokens)

            let otherDeclarators, rest = parseDeclarators error tokens
            ((declarator, assignment) :: otherDeclarators, rest)
        | rest -> ([], rest)


    let otherDeclarators, tokens = parseDeclarators error tokens

    let _, tokens =
        expect SemiColon error tokens "Expected ';' after declaration"

    ({ Declarators = (firstDeclarator, assignment) :: otherDeclarators }, tokens)

let parseRep (reporter: string -> unit) (sourceObject: SourceObject) =

    let error location message =
        sourceObject.emitError location message
        |> reporter

    let rec parseTranslationUnit (sourceObject: SourceObject) (tokens: Token list) =
        match tokens with
        | [] -> []
        | { Type = IntKeyword } :: _ ->
            let declaration, tokens = parseDeclaration error tokens

            DeclarationCompoundItem declaration
            :: parseTranslationUnit sourceObject tokens
        | _ -> failwith ""
    //            let statement, rest = parseStatement error tokens
//
//            StatementCompoundItem statement
//            :: parseTranslationUnit sourceObject rest

    parseTranslationUnit sourceObject sourceObject.Tokens


let parse = parseRep Console.Error.Write
