module Micro16C.Frontend.Parse

open System
open Micro16C.Frontend.Lex

(*
<TranslationUnit> ::= { <Statements> | <Declaration>}

<Declaration> ::= {<DeclarationSpecifier>} <Declarator> [ '=' <AssignmentExpression> ] { ',' <Declarator> [ '=' <AssignmentExpression> ] } ';'

<TypeSpecifier> ::= 'int'

<StorageSpecifier> ::= 'register' '(' IDENTIFIER ')'

<DeclarationSpecifiers> ::= <TypeSpecifier> [<StorageSpecifier>]
                          | [<StorageSpecifier>] <TypeSpecifier>

<Declarator> ::= {'*'} IDENTIFIER

<AbstractDeclarator> ::= {'*'}

<TypeName> ::= <TypeSpecifier> [<AbstractDeclarator>]

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
    { DeclarationSpecifiers: Token option
      Declarators: (Declarator * AssignmentExpression option) list }

type CompoundItem =
    | StatementCompoundItem of Statement
    | DeclarationCompoundItem of Declaration

and Statement =
    | WhileStatement of Expression * Statement
    | DoWhileStatement of Statement * Expression
    | ForStatementDecl of Declaration * Expression option * Expression option * Statement
    | ForStatement of Expression option * Expression option * Expression option * Statement
    | BreakStatement
    | ContinueStatement
    | GotoStatement of string
    | LabelStatement of string * Statement
    | CompoundStatement of CompoundItem list
    | ExpressionStatement of Expression option

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
        | { Type = ty } as t :: tokens when List.contains ty allowedTypes ->
            let rhs, tokens = subExprParse error tokens

            let others, tokens = parseOptionalRhs tokens

            ((t, rhs) :: others, tokens)
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
        | { Type = Identifier _ } as t :: tokens -> (IdentifierPrimaryExpression t, tokens)
        | { Type = Literal _ } as t :: tokens -> (LiteralPrimaryExpression t, tokens)
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

let parseDeclarationSpecifiers error (tokens: Token list) =
    match tokens with
    | { Type = IntKeyword } :: { Type = t } :: tokens when t <> RegisterKeyword -> (None, tokens)
    | { Type = IntKeyword } :: { Type = RegisterKeyword } :: tokens ->
        let _, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'register'"

        let s, tokens =
            match tokens with
            | { Type = Identifier _ } as t :: tokens -> (Some t, tokens)
            | t :: _ ->
                error (ErrorTypeToken t) "Expected register name after '(' in 'register'"
                (None, tokens)
            | [] ->
                error ErrorTypeEnd "Expected register name after '(' in 'register'"
                (None, tokens)

        let _, tokens =
            expect OpenParentheses error tokens "Expected ')' to match ')'"

        (s, tokens)
    | { Type = RegisterKeyword } :: tokens ->
        let _, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'register'"

        let s, tokens =
            match tokens with
            | { Type = Identifier _ } as t :: tokens -> (Some t, tokens)
            | t :: _ ->
                error (ErrorTypeToken t) "Expected register name after '(' in 'register'"
                (None, tokens)
            | [] ->
                error ErrorTypeEnd "Expected register name after '(' in 'register'"
                (None, tokens)

        let _, tokens =
            expect OpenParentheses error tokens "Expected ')' to match ')'"

        let _, tokens =
            expect OpenParentheses error tokens "Expected 'int' in type specifiers"

        (s, tokens)
    | _ ->
        match expect OpenParentheses error tokens "Expected 'int' in type specifiers" with
        | (_, tokens) -> (None, tokens)

let parseDeclaration error (tokens: Token list) =
    let register, tokens = parseDeclarationSpecifiers error tokens

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
               Identifier = None },
             List.tail tokens)
        | None ->
            "Expected identifier in declaration"
            |> error ErrorTypeEnd

            ({ PointerCount = pointerCount
               Identifier = None },
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

    ({ DeclarationSpecifiers = register
       Declarators = (firstDeclarator, assignment) :: otherDeclarators },
     tokens)

type private DeclOrExpr =
    | MaybeExpression of Expression option
    | Declaration of Declaration

let rec parseStatement error (tokens: Token list) =
    match tokens with
    | { Type = WhileKeyword } :: tokens ->
        let _, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'while'"

        let expression, tokens = parseExpression error tokens

        let _, tokens =
            expect CloseParentheses error tokens "Expected ')' to match '('"

        let statement, tokens = parseStatement error tokens

        (WhileStatement(expression, statement), tokens)
    | { Type = DoKeyword } :: tokens ->
        let statement, tokens = parseStatement error tokens

        let _, tokens =
            expect WhileKeyword error tokens "Expected 'while' to match 'do'"

        let _, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'while'"

        let expression, tokens = parseExpression error tokens

        let _, tokens =
            expect CloseParentheses error tokens "Expected ')' to match '('"

        (DoWhileStatement(statement, expression), tokens)
    | { Type = ForKeyword } :: tokens ->

        let _, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'for'"

        let first, tokens =
            match tokens with
            | { Type = IntKeyword } :: _
            | { Type = RegisterKeyword } :: _ ->
                let decl, tokens = parseDeclaration error tokens
                (Declaration decl, tokens)
            | { Type = SemiColon } :: tokens -> (MaybeExpression None, tokens)
            | _ ->
                let expr, tokens = parseExpression error tokens
                (MaybeExpression(Some expr), tokens)

        let second, tokens =
            match tokens with
            | { Type = SemiColon } :: tokens -> (None, tokens)
            | _ ->
                let expr, tokens = parseExpression error tokens
                (Some expr, tokens)

        let third, tokens =
            match tokens with
            | { Type = CloseParentheses } :: tokens -> (None, tokens)
            | _ ->
                let expr, tokens = parseExpression error tokens
                (Some expr, tokens)

        let _, tokens =
            expect CloseParentheses error tokens "Expected ')' to match '('"

        let statement, tokens = parseStatement error tokens

        match first with
        | MaybeExpression maybeExpr -> (ForStatement(maybeExpr, second, third, statement), tokens)
        | Declaration decl -> (ForStatementDecl(decl, second, third, statement), tokens)

    | { Type = BreakKeyword } :: tokens ->
        let _, tokens =
            expect SemiColon error tokens "Expected ';' after 'break'"

        (BreakStatement, tokens)

    | { Type = ContinueKeyword } :: tokens ->
        let _, tokens =
            expect SemiColon error tokens "Expected ';' after 'continue'"

        (ContinueStatement, tokens)

    | { Type = GotoKeyword } :: tokens ->
        let s, tokens =
            match tokens with
            | { Type = Identifier s } :: tokens -> (s, tokens)
            | [] ->
                error ErrorTypeEnd "Expected identifier after 'goto'"
                ("", [])
            | t :: _ ->
                error (ErrorTypeToken t) "Expected identifier after 'goto'"
                ("", tokens)

        let _, tokens =
            expect SemiColon error tokens "Expected ';' after identifier in 'goto'"

        (GotoStatement s, tokens)

    | { Type = Identifier s } :: { Type = Comma } :: tokens ->
        let statement, tokens = parseStatement error tokens
        (LabelStatement(s, statement), tokens)

    | { Type = OpenBrace } :: tokens ->
        let compoundItems, tokens = parseCompoundItems error tokens
        (CompoundStatement compoundItems, tokens)
    | { Type = SemiColon } :: tokens -> (ExpressionStatement None, tokens)
    | _ ->
        let expression, tokens = parseExpression error tokens
        (ExpressionStatement(Some expression), tokens)

and parseCompoundItems error (tokens: Token list) =
    match tokens with
    | { Type = IntKeyword } :: _
    | { Type = RegisterKeyword } :: _ ->
        let declaration, tokens = parseDeclaration error tokens

        let other, tokens = parseCompoundItems error tokens

        (DeclarationCompoundItem declaration :: other, tokens)
    | { Type = WhileKeyword } :: _
    | { Type = DoKeyword } :: _
    | { Type = BreakKeyword } :: _
    | { Type = ContinueKeyword } :: _
    | { Type = GotoKeyword } :: _
    | { Type = Identifier _ } :: _
    | { Type = Literal _ } :: _
    | { Type = OpenParentheses } :: _
    | { Type = Increment } :: _
    | { Type = Decrement } :: _
    | { Type = Ampersand } :: _
    | { Type = Asterisk } :: _
    | { Type = Plus } :: _
    | { Type = Minus } :: _
    | { Type = LogicalNegation } :: _
    | { Type = BitWiseNegation } :: _
    | { Type = SizeOfKeyword } :: _
    | { Type = OpenBrace } :: _
    | { Type = SemiColon } :: _
    | { Type = ForKeyword } :: _ ->
        let statement, tokens = parseStatement error tokens

        let other, tokens = parseCompoundItems error tokens

        (StatementCompoundItem statement :: other, tokens)
    | _ -> ([], tokens)

let parseRep (reporter: string -> unit) (sourceObject: SourceObject) =

    let error location message =
        sourceObject.emitError location message
        |> reporter

    parseCompoundItems error sourceObject.Tokens


let parse = parseRep Console.Error.Write
