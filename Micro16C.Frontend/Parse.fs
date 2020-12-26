module Micro16C.Frontend.Parse

open Micro16C.Frontend.Lex
open Micro16C.Frontend.ErrorHandling

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
      Identifier: Token }

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
      OptionalExclusiveOrExpressions: (Token * ExclusiveOrExpression) list }

and ExclusiveOrExpression =
    { AndExpression: AndExpression
      OptionalAndExpressions: (Token * AndExpression) list }

and AndExpression =
    { EqualityExpression: EqualityExpression
      OptionalEqualityExpressions: (Token * EqualityExpression) list }

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
    | IncrementUnaryExpression of UnaryExpression * Token
    | DecrementUnaryExpression of UnaryExpression * Token
    | AddressOfUnaryExpression of UnaryExpression * Token
    | DereferenceUnaryExpression of UnaryExpression * Token
    | PlusUnaryExpression of UnaryExpression * Token
    | MinusUnaryExpression of UnaryExpression * Token
    | BitwiseNegateUnaryExpression of UnaryExpression * Token
    | LogicalNegateUnaryExpression of UnaryExpression * Token
    | SizeOfUnaryExpression of UnaryExpression
    | SizeOfTypeUnaryExpression of TypeName

and PostFixExpression =
    | PrimaryPostFixExpression of PrimaryExpression
    | SubscriptPostFixExpression of PostFixExpression * Token * Expression
    | IncrementPostFixExpression of PostFixExpression * Token
    | DecrementPostFixExpression of PostFixExpression * Token

and PrimaryExpression =
    | IdentifierPrimaryExpression of Token
    | LiteralPrimaryExpression of Token
    | ExpressionPrimaryExpression of Expression

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
    | BreakStatement of Token
    | ContinueStatement of Token
    | GotoStatement of string
    | LabelStatement of string * Statement
    | CompoundStatement of CompoundItem list
    | ExpressionStatement of Expression option

let private createExpression assignment optionalAssignments =
    { AssignmentExpression = assignment
      OptionalAssignmentExpressions = optionalAssignments }

let private createAssignmentExpression conditional optionalConditional =
    { ConditionalExpression = conditional
      OptionalConditionalExpressions = optionalConditional }

let private createConditionalExpression logicalExpression optionalTernary =
    { LogicalOrExpression = logicalExpression
      OptionalTernary = optionalTernary }

let private createLogicalOrExpression logicalAnd optionalLogicalAnd =
    { LogicalAndExpression = logicalAnd
      OptionalLogicalAndExpressions = optionalLogicalAnd }

let private createLogicalAndExpression inclusiveOr optionalInclusiveOr =
    { InclusiveOrExpression = inclusiveOr
      OptionalInclusiveOrExpressions = optionalInclusiveOr }

let private createInclusiveOrExpression exclusiveOr optionalExclusiveOr =
    { ExclusiveOrExpression = exclusiveOr
      OptionalExclusiveOrExpressions = optionalExclusiveOr }

let private createExclusiveOrExpression andExpression optionalAnd =
    { AndExpression = andExpression
      OptionalAndExpressions = optionalAnd }

let private createAndExpression equality optionalEquality =
    { EqualityExpression = equality
      OptionalEqualityExpressions = optionalEquality }

let private createEqualityExpression relational optionalRelational =
    { RelationalExpression = relational
      OptionalRelationalExpressions = optionalRelational }

let private createRelationalExpression shift optionalShift =
    { ShiftExpression = shift
      OptionalShiftExpressions = optionalShift }

let private createShiftExpression additive optionalAdditive =
    { AdditiveExpression = additive
      OptionalAdditiveExpressions = optionalAdditive }

let private createAdditiveExpression multiplicative optionalMultiplicative =
    { MultiplicativeExpression = multiplicative
      OptionalMultiplicativeExpressions = optionalMultiplicative }

let private createMultiplicativeExpression unary optionalUnary =
    { UnaryExpression = unary
      OptionalUnaryExpressions = optionalUnary }

let private createDeclaration declarationSpecifiers declarators =
    { DeclarationSpecifiers = declarationSpecifiers
      Declarators = declarators }

let inline private parseBinaryOperator subExprParse allowedType error tokens =
    let lhs, tokens = subExprParse error tokens

    let rec parseOptionalRhs tokens =
        match tokens with
        | { Type = t } :: tokens when t = allowedType ->
            let rhs, tokens = subExprParse error tokens

            let others, tokens = parseOptionalRhs tokens

            (prependResult rhs others, tokens)
        | _ -> ([] |> Ok, tokens)

    let optionalRhs, tokens = parseOptionalRhs tokens

    (lhs, optionalRhs, tokens)

let private expect tokenType error (tokens: Token list) message: Result<Token, string> * Token list =
    match List.tryHead tokens with
    | Some { Type = t } when t = tokenType -> (List.head tokens |> Ok, List.tail tokens)
    | Some t -> (message |> error (ErrorTypeToken t) |> Error, tokens)
    | None -> (message |> error ErrorTypeEnd |> Error, tokens)

let private parseBinaryMultiOperator subExprParse allowedTypes error (tokens: Token list) =
    let lhs, tokens = subExprParse error tokens

    let rec parseOptionalRhs (tokens: Token list) =
        match tokens with
        | { Type = ty } as t :: tokens when List.contains ty allowedTypes ->
            let rhs, tokens = subExprParse error tokens

            let others, tokens = parseOptionalRhs tokens

            (comb2 (fun rhs others -> ((t, rhs) :: others)) rhs others, tokens)
        | _ -> ([] |> Ok, tokens)

    let optionalRhs, tokens = parseOptionalRhs tokens

    (lhs, optionalRhs, tokens)

let rec parseExpression error (tokens: Token list) =

    let lhs, rhs, tokens =
        parseBinaryOperator parseAssignmentExpression Comma error tokens

    (comb2 createExpression lhs rhs, tokens)

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

    (comb2 createAssignmentExpression lhs rhs, tokens)

and parseConditionalExpression error (tokens: Token list) =
    let logicalOrExpression, tokens = parseLogicalOrExpression error tokens

    match tokens with
    | { Type = QuestionMark } :: tokens ->
        let expression, tokens = parseExpression error tokens

        match expect Colon error tokens "Expected ':' to match '?'" with
        | (Ok _, tokens) ->
            let conditionalExpression, tokens = parseConditionalExpression error tokens

            let optionalTernary =
                comb2 pack2 expression conditionalExpression
                |> Result.map Some

            (comb2 createConditionalExpression logicalOrExpression optionalTernary, tokens)
        | (error1, tokens) ->
            (comb3 (fun _ -> createConditionalExpression) error1 logicalOrExpression (Ok None), tokens)
    | _ -> (comb2 createConditionalExpression logicalOrExpression (Ok None), tokens)

and parseLogicalOrExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryOperator parseLogicalAndExpression LogicOr error tokens

    (comb2 createLogicalOrExpression lhs rhs, tokens)

and parseLogicalAndExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryOperator parseInclusiveOrExpression LogicAnd error tokens

    (comb2 createLogicalAndExpression lhs rhs, tokens)

and parseInclusiveOrExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseExclusiveOrExpression [ BitOr ] error tokens

    (comb2 createInclusiveOrExpression lhs rhs, tokens)

and parseExclusiveOrExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseAndExpression [ BitXor ] error tokens

    (comb2 createExclusiveOrExpression lhs rhs, tokens)

and parseAndExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseEqualityExpression [ Ampersand ] error tokens

    (comb2 createAndExpression lhs rhs, tokens)

and parseEqualityExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseRelationalExpression [ Equal; NotEqual ] error tokens

    (comb2 createEqualityExpression lhs rhs, tokens)

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

    (comb2 createRelationalExpression lhs rhs, tokens)

and parseShiftExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseAdditiveExpression [ ShiftRight; ShiftLeft ] error tokens

    (comb2 createShiftExpression lhs rhs, tokens)

and parseAdditiveExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseMultiplicativeExpression [ Plus; Minus ] error tokens

    (comb2 createAdditiveExpression lhs rhs, tokens)

and parseMultiplicativeExpression error (tokens: Token list) =
    let lhs, rhs, tokens =
        parseBinaryMultiOperator parseUnaryExpression [ Asterisk; Division; Percent ] error tokens

    (comb2 createMultiplicativeExpression lhs rhs, tokens)

and parseUnaryExpression error (tokens: Token list) =
    match tokens with
    | { Type = Increment } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> IncrementUnaryExpression(x, token)),
         tokens)
    | { Type = Decrement } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> DecrementUnaryExpression(x, token)),
         tokens)
    | { Type = Ampersand } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> AddressOfUnaryExpression(x, token)),
         tokens)
    | { Type = Asterisk } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> DereferenceUnaryExpression(x, token)),
         tokens)
    | { Type = Plus } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> PlusUnaryExpression(x, token)),
         tokens)
    | { Type = Minus } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> MinusUnaryExpression(x, token)),
         tokens)
    | { Type = BitWiseNegation } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> BitwiseNegateUnaryExpression(x, token)),
         tokens)
    | { Type = LogicalNegation } as token :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens

        (unary
         |> Result.map (fun x -> LogicalNegateUnaryExpression(x, token)),
         tokens)
    | { Type = SizeOfKeyword } :: { Type = OpenParentheses } :: { Type = IntKeyword } :: tokens ->
        let pointerCount =
            tokens
            |> List.takeWhile (function
                | { Type = Asterisk } -> true
                | _ -> false)
            |> List.length

        let tokens = tokens |> List.skip pointerCount

        (SizeOfTypeUnaryExpression { PointerCount = pointerCount }
         |> Ok,
         tokens)
    | { Type = SizeOfKeyword } :: tokens ->
        let unary, tokens = parseUnaryExpression error tokens
        (unary |> Result.map SizeOfUnaryExpression, tokens)
    | _ ->
        let postFix, tokens = parsePostFixExpression error tokens
        (postFix |> Result.map PostFixUnaryExpression, tokens)

and parsePostFixExpression error (tokens: Token list) =
    let primary, tokens =
        match tokens with
        | { Type = Identifier _ } as t :: tokens -> (IdentifierPrimaryExpression t |> Ok, tokens)
        | { Type = Literal _ } as t :: tokens -> (LiteralPrimaryExpression t |> Ok, tokens)
        | { Type = OpenParentheses } :: tokens ->
            let expression, tokens = parseExpression error tokens

            let error, tokens =
                expect CloseParentheses error tokens "Expected ')' to match '('"

            (comb2 (fun _ -> ExpressionPrimaryExpression) error expression, tokens)
        | [] ->
            ("Expected Identifier, Literal or '('"
             |> error ErrorTypeEnd
             |> Error,
             tokens)
        | t :: _ ->
            ("Expected Identifier, Literal or '('"
             |> error (ErrorTypeToken t)
             |> Error,
             tokens)

    let rec parsePostFixExpressionSuffix prev (tokens: Token list) =

        match tokens with
        | { Type = OpenSquareBracket } :: tokens ->
            let expression, tokens = parseExpression error tokens

            let error1, tokens =
                expect CloseSquareBracket error tokens "Expected ']' to match '['"

            parsePostFixExpressionSuffix
                (comb3 (fun x y z -> SubscriptPostFixExpression(x, y, z)) prev error1 expression)
                tokens
        | { Type = Increment } as token :: tokens ->
            parsePostFixExpressionSuffix
                (prev
                 |> Result.map (fun x -> IncrementPostFixExpression(x, token)))
                tokens
        | { Type = Decrement } as token :: tokens ->
            parsePostFixExpressionSuffix
                (prev
                 |> Result.map (fun x -> DecrementPostFixExpression(x, token)))
                tokens
        | _ -> (prev, tokens)

    parsePostFixExpressionSuffix (primary |> Result.map PrimaryPostFixExpression) tokens

let parseDeclarationSpecifiers error (tokens: Token list) =
    match tokens with
    | { Type = IntKeyword } :: ({ Type = t } as head) :: tokens when t <> RegisterKeyword ->
        (None |> Ok, head :: tokens)
    | { Type = IntKeyword } :: { Type = RegisterKeyword } :: tokens ->
        let error1, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'register'"

        let s, tokens =
            match tokens with
            | { Type = Identifier _ } as t :: tokens -> (Ok t, tokens)
            | t :: _ ->
                (error (ErrorTypeToken t) "Expected register name after '(' in 'register'"
                 |> Error,
                 tokens)
            | [] ->
                (error ErrorTypeEnd "Expected register name after '(' in 'register'"
                 |> Error,
                 tokens)

        let error2, tokens =
            expect CloseParentheses error tokens "Expected ')' to match ')'"

        (comb3 (fun _ _ -> Some) error1 error2 s, tokens)
    | { Type = RegisterKeyword } :: tokens ->
        let error1, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'register'"

        let s, tokens =
            match tokens with
            | { Type = Identifier _ } as t :: tokens -> (Ok t, tokens)
            | t :: _ ->
                (error (ErrorTypeToken t) "Expected register name after '(' in 'register'"
                 |> Error,
                 tokens)
            | [] ->
                (error ErrorTypeEnd "Expected register name after '(' in 'register'"
                 |> Error,
                 tokens)

        let error2, tokens =
            expect CloseParentheses error tokens "Expected ')' to match ')'"

        let error3, tokens =
            expect IntKeyword error tokens "Expected 'int' in type specifiers"

        (comb4 (fun _ _ _ -> Some) error1 error2 error3 s, tokens)
    | _ ->
        let error, tokens =
            expect IntKeyword error tokens "Expected 'int' in type specifiers"

        (error |> Result.map Some, tokens)

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
        | Some ({ Type = Identifier _ } as t) ->
            ({ PointerCount = pointerCount
               Identifier = t }
             |> Ok,
             List.tail tokens)
        | Some t ->
            ("Expected identifier in declaration"
             |> error (ErrorTypeToken t)
             |> Error,
             List.tail tokens)
        | None ->
            ("Expected identifier in declaration"
             |> error ErrorTypeEnd
             |> Error,
             [])

    let firstDeclarator, tokens = parseDeclarator tokens

    let assignment, tokens =
        match tokens with
        | { Type = Assignment } :: tokens ->
            let assignment, tokens = parseAssignmentExpression error tokens
            (assignment |> Result.map Some, tokens)
        | _ -> (None |> Ok, tokens)

    let rec parseDeclarators error (tokens: Token list) =
        match tokens with
        | { Type = Comma } :: tokens ->
            let declarator, tokens = parseDeclarator tokens

            let assignment, tokens =
                match tokens with
                | { Type = Assignment } :: tokens ->
                    let assignment, tokens = parseAssignmentExpression error tokens
                    (assignment |> Result.map Some, tokens)
                | _ -> (None |> Ok, tokens)

            let otherDeclarators, rest = parseDeclarators error tokens

            (comb3 (fun x y z -> (x, y) :: z) declarator assignment otherDeclarators, rest)
        | rest -> ([] |> Ok, rest)


    let otherDeclarators, tokens = parseDeclarators error tokens

    let error1, tokens =
        expect SemiColon error tokens "Expected ';' after declaration"

    match error1 with
    | Error s -> (Error s, tokens)
    | Ok _ ->
        (comb4 (fun x y z w -> createDeclaration x ((y, w) :: z)) register firstDeclarator otherDeclarators assignment,
         tokens)


type private DeclOrExpr =
    | MaybeExpression of Expression option
    | Declaration of Declaration

let rec parseStatement error (tokens: Token list) =
    match tokens with
    | { Type = WhileKeyword } :: tokens ->
        let error1, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'while'"

        let expression, tokens = parseExpression error tokens

        let error2, tokens =
            expect CloseParentheses error tokens "Expected ')' to match '('"

        let statement, tokens = parseStatement error tokens

        (comb4 (fun _ _ x y -> WhileStatement(x, y)) error1 error2 expression statement, tokens)
    | { Type = DoKeyword } :: tokens ->
        let statement, tokens = parseStatement error tokens

        let error1, tokens =
            expect WhileKeyword error tokens "Expected 'while' to match 'do'"

        let error2, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'while'"

        let expression, tokens = parseExpression error tokens

        let error3, tokens =
            expect CloseParentheses error tokens "Expected ')' to match '('"

        let error123 =
            comb3 (fun _ _ _ -> ()) error1 error2 error3

        (comb3 (fun _ statement expression -> DoWhileStatement(statement, expression)) error123 statement expression,
         tokens)
    | { Type = ForKeyword } :: tokens ->

        let error1, tokens =
            expect OpenParentheses error tokens "Expected '(' after 'for'"

        let first, tokens =
            match tokens with
            | { Type = IntKeyword } :: _
            | { Type = RegisterKeyword } :: _ ->
                let decl, tokens = parseDeclaration error tokens
                (decl |> Result.map Declaration, tokens)
            | { Type = SemiColon } :: tokens -> (MaybeExpression None |> Ok, tokens)
            | _ ->
                let expr, tokens = parseExpression error tokens
                (expr |> Result.map (Some >> MaybeExpression), tokens)

        let second, tokens =
            match tokens with
            | { Type = SemiColon } :: tokens -> (None |> Ok, tokens)
            | _ ->
                let expr, tokens = parseExpression error tokens
                (expr |> Result.map Some, tokens)

        let third, tokens =
            match tokens with
            | { Type = CloseParentheses } :: tokens -> (None |> Ok, tokens)
            | _ ->
                let expr, tokens = parseExpression error tokens
                (expr |> Result.map Some, tokens)

        let error2, tokens =
            expect CloseParentheses error tokens "Expected ')' to match '('"

        let statement, tokens = parseStatement error tokens

        match (error1, error2) with
        | (Error s1, Error s2) -> (s1 + s2 |> Error, tokens)
        | (Error s, Ok _) -> (Error s, tokens)
        | (Ok _, Error s) -> (Error s, tokens)
        | _ ->
            (comb4 (fun first second third statement ->
                match first with
                | MaybeExpression maybeExpr -> ForStatement(maybeExpr, second, third, statement)
                | Declaration decl -> ForStatementDecl(decl, second, third, statement)) first second third statement,
             tokens)

    | { Type = BreakKeyword } as token :: tokens ->
        match expect SemiColon error tokens "Expected ';' after 'break'" with
        | (Error s, tokens) -> (Error s, tokens)
        | (Ok _, tokens) -> (BreakStatement token |> Ok, tokens)

    | { Type = ContinueKeyword } as token :: tokens ->
        match expect SemiColon error tokens "Expected ';' after 'continue'" with
        | (Error s, tokens) -> (Error s, tokens)
        | (Ok _, tokens) -> (ContinueStatement token |> Ok, tokens)

    | { Type = GotoKeyword } :: tokens ->
        let s, tokens =
            match tokens with
            | { Type = Identifier s } :: tokens -> (s |> Ok, tokens)
            | [] ->
                (error ErrorTypeEnd "Expected identifier after 'goto'"
                 |> Error,
                 [])
            | t :: _ ->
                (error (ErrorTypeToken t) "Expected identifier after 'goto'"
                 |> Error,
                 tokens)

        let error1, tokens =
            expect SemiColon error tokens "Expected ';' after identifier in 'goto'"

        (comb2 (fun _ -> GotoStatement) error1 s, tokens)

    | { Type = Identifier s } :: { Type = Comma } :: tokens ->
        let statement, tokens = parseStatement error tokens

        (statement
         |> Result.map (fun statement -> LabelStatement(s, statement)),
         tokens)

    | { Type = OpenBrace } :: tokens ->
        let compoundItems, tokens = parseCompoundItems error tokens

        let error1, tokens =
            expect CloseBrace error tokens "Expected '}' to match '{'"

        (comb2 (fun _ -> CompoundStatement) error1 compoundItems, tokens)
    | { Type = SemiColon } :: tokens -> (ExpressionStatement None |> Ok, tokens)
    | _ ->
        let expression, tokens = parseExpression error tokens

        let error1, tokens =
            expect SemiColon error tokens "Expected ';' after expression statement"

        (comb2 (fun _ -> Some >> ExpressionStatement) error1 expression, tokens)

and parseCompoundItems error (tokens: Token list) =
    match tokens with
    | { Type = IntKeyword } :: _
    | { Type = RegisterKeyword } :: _ ->
        let declaration, tokens = parseDeclaration error tokens

        let other, tokens = parseCompoundItems error tokens

        (prependResult (declaration |> Result.map DeclarationCompoundItem) other, tokens)
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

        (prependResult (statement |> Result.map StatementCompoundItem) other, tokens)
    | _ -> ([] |> Ok, tokens)

let parse (sourceObject: SourceObject) =

    let error location message = sourceObject.emitError location message

    (parseCompoundItems error sourceObject.Tokens
     |> fst
     |> Result.map (pack2 { sourceObject with Tokens = [] }))
