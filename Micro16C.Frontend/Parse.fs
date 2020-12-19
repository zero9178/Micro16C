module Micro16C.Frontend.Parse

(*
<TranslationUnit> ::= { <Statements> | <Declaration>}

<Declaration> ::= <DeclarationSpecifiers> <Declarator> [ '=' <AssignmentExpression> ] { ',' <Declarator> [ '=' <AssignmentExpression> ] } ';'

<DeclarationSpecifiers> ::= 'int'

<Declarator> ::= {'*'} IDENTIFIER

<AbstractDeclarator> ::= {'*'}

<TypeName> ::= <DeclarationSpecifiers> [<AbstractDeclarator>]

<Expression> ::= <AssignmentExpression> ',' <AssignmentExpression>

<AssignmentExpression> ::= <ConditionalExpression> <AssignmentOperator> <ConditionalExpression>

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

<Statements> ::= <IfStatement>
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

let parse tokens = ()
