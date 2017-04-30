:- module(main, [parse/3]).

/*
    DCG Parser of HDML language for veryfing digital circuits
*/

/*
    BNF Grammar:

    <program>       ::=     <definitions>
    <definitions>   ::=     <empty>
                     |      <definition> <definitions>
    <definition>    ::=     def <identifier> ( <pattern> ) = <expr>
    <pattern>       ::=     _
                     |      <variable>
                     |      ( <pattern> )
                     |      <pattern> , <pattern>
    <expr>          ::=     if <expr> then <expr> else <expr>
                     |      let <pattern> = <expr> in <expr>
                     |      <expr_op>
    <expr_op>       ::=     <expr_op> <binary_op> <expr_op>
                     |      <unary_op> <expr_op>
                     |      <simple_expr>
    <binary_op>     ::=     , | = | <> | < | > | <= | >=
                     |      ^ | | | + | - | & | * | / | % | @
    <unary_op>      ::=     - | # | ~
    <simple_expr>   ::=     ( <expr> )
                     |      <bit_select>
                     |      <bits_select>
                     |      <atomic_expr>
    <bit_select>    ::=     <simple_expr> [ <expr> ]
    <bits_select>   ::=     <simple_expr> [ <expr> .. <expr> ]
    <atomic_expr>   ::=     <variable>
                     |      <func_exec>
                     |      <int_literal>
                     |      <empty_vector>
                     |      <single_bit>
    <variable>      ::=     <identifier>
    <func_exec>     ::=     <identifier> ( <expr> )
    <empty_vector>  ::=     [ ]
    <single_bit>    ::=     [ <expr> ]
    <empty>         ::=


    For convenience and readability, <expr_op> definition is shortcutted.
    Here is full operator-precedence-aware grammar for <expr_op>:

    <expr_op>       ::=     <expr_op1> <binary_op1  > <expr_op > | <expr_op1>
    <expr_op1>      ::=     <expr_op2> <binary_op2  > <expr_op2> | <expr_op2>
    <expr_op2>      ::=     <expr_op3> <binary_op3_r> <expr_op2> | <expr_op3>
    <expr_op3>      ::=     <expr_op3> <binary_op3_l> <expr_op4> | <expr_op4>
    <expr_op4>      ::=     <expr_op4> <binary_op5  > <expr_op_rest>
    <expr_op_rest>  ::=     <unary_op> <unary_expr>
                     |      <simple_expr>

    <unary_expr>    ::=     <unary_op> <simple_expr>
    <unary_expr>    ::=     <simple_expr>

*/

/*
    OPERATORS PRIORITY AND ASSOCIATIVITY

    Operators       | Associativity | Priority
    -------------------------------------
    & * / %         | left          | 5
    | ^ + -         | left          | 3
    @               | right         | 3
    = <> < > <= >=  | none          | 2
    ,               | right         | 1

*/

/*
    EXAMPLE

    Program in HDML:

        def half_adder(A, B) =
            A&B, A^B

    Parsing result,
    Abstract Syntax Tree (as a prolog list):

        [ def(half_adder, pair(no, var(no, 'A'), var(no, 'B')),
          pair(no, op(no, '&', var(no, 'A'), var(no, 'B')),
          op(no, '^', var(no, 'A'), var(no, 'B')))) ]

*/


/*
* LEXICAL ANALYSIS -- LEXER
*
* */


lexer(Tokens) -->
    white_space,
    (( "&",     !, { Token = tokenAnd }
     ; "*",     !, { Token = tokenMult }
     ; "/",     !, { Token = tokenDiv }
     ; "%",     !, { Token = tokenMod }
     ; "|",     !, { Token = tokenPipe }
     ; "^",     !, { Token = tokenCaret }
     ; "+",     !, { Token = tokenPlus }
     ; "-",     !, { Token = tokenMinus }
     ; "@",     !, { Token = tokenAt }
     ; "=",     !, { Token = tokenAssign }
     ; "<>",    !, { Token = tokenNe }
     ; "<=",    !, { Token = tokenLte }
     ; ">=",    !, { Token = tokenGte }
     ; "<",     !, { Token = tokenLt }
     ; ">",     !, { Token = tokenGt }
     ; "..",    !, { Token = tokenDoubleDot }
     ; ",",     !, { Token = tokenComma }
     ; "(",     !, { Token = tokenLParen }
     ; ")",     !, { Token = tokenRParen }
     ; "[",     !, { Token = tokenLBracket }
     ; "]",     !, { Token = tokenRBracket }
     ; "#",     !, { Token = tokenHash }
     ; "~",     !, { Token = tokenTilde }
     ; digit(D),
         number(D, N),
         { Token = tokenNum(N) }
     ; letter(L), !, identifier(L, Id),
         { member((Id, Token), [(def, tokenDef),
                                (else, tokenElse),
                                (if, tokenIf),
                                (in, tokenIn),
                                (let, tokenLet),
                                (then, tokenThen),
                                ('_', tokenWildcard)]),
           !
         ; Token = tokenVar(Id)
         }
     ),
     !,
     { Tokens = [Token | TokenList] },
     lexer(TokenList)
    ; [],
        { Tokens = [] }
    ).


white_space --> comment, !, white_space.
white_space -->
    [Char], { code_type(Char, space) }, !, white_space.
white_space -->
    [].

comment -->
    [L, M], {
        char_code('(', L),
        char_code('*', M)
    }, !,
    comment_contents.

comment_contents -->
    comment, !,
    comment_contents.

comment_contents -->
    [M, P], {
        char_code('*', M),
        char_code(')', P)
    }, !.
comment_contents --> [_], !, comment_contents.
comment_contents --> [].


digit(Digit) -->
    [Digit], { code_type(Digit, digit) }.

digits([D|T]) -->
    digit(D), !,
    digits(T).

digits([]) --> [].


number(D, N) -->
    digits(Ds),
    { number_chars(N, [D|Ds]) }.


is_underscore(X) :-
    integer(X), !,
    char_code('_', X).

is_underscore(X) :-
    char_code(X, 95).


letter(Letter) -->
    [Letter], { is_underscore(Letter) }.

letter(Letter) -->
    [Letter], { code_type(Letter, alpha) }.

alphanum([A|T]) -->
    [A], {(
        code_type(A, alnum);
        is_underscore(A);
        char_code("'", A)
    )}, !, alphanum(T).


alphanum([]) --> [].

identifier(L, Id) -->
    alphanum(As),
    { atom_codes(Id, [L|As]) }.



/*
SYNTAX ANALYSIS -- PARSER
BNF grammar listed at the beggining of the file

*Left recursion* notes are left in places where grammar would go into
infinite loops, thus, needs to be resolved in a slightly modified way

*/


% Parser

program(AST) -->
    definitions(AST).


definitions(Defs) -->
    definition(DefHead), !,
    definitions(DefTail),
    { Defs = [DefHead | DefTail] }.

definitions([]) --> [].


definition(Def) -->
    [tokenDef], [tokenVar(Id)],
    [tokenLParen], pattern(Pattern), [tokenRParen],
    [tokenAssign], expr(Expr),
    { Def = def(Id, Pattern, Expr) }.


% Left recursion

pattern(Pattern) -->
    (   [tokenWildcard], !,
        { PatternAcc = wildcard(no) }

    ;   variable(Var), !,
        { PatternAcc = Var }

    ;   [tokenLParen], !, pattern(PatternAcc), [tokenRParen]
    ),
    pattern(PatternAcc, Pattern).


pattern(Acc, Pattern) -->
    [tokenComma], !,
    pattern(Pattern1),
    { Pattern = pair(no, Acc, Pattern1) }.

pattern(Acc, Acc) --> [].


expr(Expr) -->
    (   [tokenIf], expr(Expr1), !,
        [tokenThen], expr(Expr2),
        [tokenElse], expr(Expr3),
        { Expr = if(no, Expr1, Expr2, Expr3) }

    ;   [tokenLet], pattern(Pattern), !,
        [tokenAssign], expr(Expr1),
        [tokenIn], expr(Expr2),
        { Expr = let(no, Pattern, Expr1, Expr2) }
    ;   expr_op(Expr)
    ).


expr_op(ExprOp) -->
    expr_op1(ExprOp1),
    binary_op_comma(_), !,
    expr_op(ExprOp2),
    { ExprOp = pair(no, ExprOp1, ExprOp2) }.

expr_op(ExprOp) -->
    expr_op1(ExprOp).

expr_op1(ExprOp) -->
    expr_op2(ExprOp1),
    binary_op2(BinaryOp), !,
    expr_op2(ExprOp2),
    { ExprOp = op(no, BinaryOp, ExprOp1, ExprOp2) }.

expr_op1(ExprOp) -->
    expr_op2(ExprOp).


expr_op2(ExprOp) -->
    expr_op3(ExprOp1),
    binary_op3_r(BinaryOp), !,
    expr_op2(ExprOp2),
    { ExprOp = op(no, BinaryOp, ExprOp1, ExprOp2) }.

expr_op2(ExprOp) -->
    expr_op3(ExprOp).

% Left recursion

expr_op3(ExprOp) -->
    expr_op4(ExprOpAcc),
    expr_op3(ExprOpAcc, ExprOp).

expr_op3(Acc, ExprOp) -->
    binary_op3_l(BinaryOp), !,
    expr_op3(ExprOp2),
    { ExprOp = op(no, BinaryOp, Acc, ExprOp2) }.

expr_op3(Acc, Acc) --> [].

% Left recursion

expr_op4(ExprOp) -->
    expr_op_rest(ExprOpAcc),
    expr_op4(ExprOpAcc, ExprOp).

expr_op4(Acc, ExprOp) -->
    binary_op5(BinaryOp),
    expr_op4(ExprOp1),
    { ExprOp = op(no, BinaryOp, Acc, ExprOp1) }.

expr_op4(Acc, Acc) --> [].


expr_op_rest(ExprOp) -->
    (   unary_op(UnaryOp), unary_expr(ExprOp1),
        { ExprOp = op(no, UnaryOp, ExprOp1) }

    ;   simple_expr(ExprOp)
    ).

unary_expr(Expr) --> simple_expr(Expr), !.
unary_expr(Expr) -->
    unary_op(UnaryOp),
    unary_expr(SimpleExpr),
    { Expr = op(no, UnaryOp, SimpleExpr) }.


% For operators associativity and priority, see description above,
% at the beggining of the file
binary_op_comma(',') --> [tokenComma], !.

binary_op2(=) --> [tokenAssign], !.
binary_op2(<>) --> [tokenNe], !.
binary_op2(<) --> [tokenLt], !.
binary_op2(>) --> [tokenGt], !.
binary_op2(<=) --> [tokenLte], !.
binary_op2(>=) --> [tokenGte], !.

% 3_r => 3rd priority, right associativity
binary_op3_r(@) --> [tokenAt], !.

% 3_l => 3rd priority, left associativity
binary_op3_l('|') --> [tokenPipe], !.
binary_op3_l(^) --> [tokenCaret], !.
binary_op3_l(+) --> [tokenPlus], !.
binary_op3_l(-) --> [tokenMinus], !.

binary_op5(&) --> [tokenAnd], !.
binary_op5(*) --> [tokenMult], !.
binary_op5(/) --> [tokenDiv], !.
binary_op5('%') --> [tokenMod], !.


unary_op(-) --> [tokenMinus], !.
unary_op(#) --> [tokenHash], !.
unary_op(~) --> [tokenTilde], !.


% Left Recursion
% Part of bit_select and bits_select logic is managed in simple_expr

simple_expr(Expr) -->
    (   [tokenLParen], !, expr(ExprAcc), [tokenRParen],
        simple_expr(ExprAcc, Expr)
    ;   atomic_expr(ExprAcc),
        simple_expr(ExprAcc, Expr)
    ).

simple_expr(Acc, Expr) -->
    bits_select(SelectExpr1, SelectExpr2), !,
    { Acc1 = bitsel(no, Acc, SelectExpr1, SelectExpr2) },
    simple_expr(Acc1, Expr).

simple_expr(Acc, Expr) -->
    bit_select(Select),
    { Acc1 = bitsel(no, Acc, Select) },
    simple_expr(Acc1, Expr).

simple_expr(Acc, Acc) --> [].


bit_select(SelectExpr) -->
    [tokenLBracket],
    expr(SelectExpr),
    [tokenRBracket].

bits_select(SelectExpr1, SelectExpr2) -->
    [tokenLBracket],
    expr(SelectExpr1),
    [tokenDoubleDot],
    expr(SelectExpr2),
    [tokenRBracket].


atomic_expr(Expr) -->
    (   func_exec(Expr), !
    ;   variable(Expr), !
    ;   empty_vector(Expr), !
    ;   int_literal(Expr), !
    ;   single_bit(Expr), !
    ).

int_literal(IntLiteral) -->
    [tokenNum(Num)],
    { IntLiteral = num(no, Num) }.

variable(ParsedVar) -->
    [tokenVar(Var)],
    { ParsedVar = var(no, Var) }.


func_exec(Func) -->
    [tokenVar(Id)],
    [tokenLParen],
    expr(Expr),
    [tokenRParen],
    { Func = call(no, Id, Expr) }.


empty_vector(Expr) -->
    [tokenLBracket],
    [tokenRBracket],
    { Expr = empty(no) }.


single_bit(Bit) -->
    [tokenLBracket],
    expr(Expr),
    [tokenRBracket],
    { Bit = bit(no, Expr) }.


empty(Expr) -->
    [],
    { Expr = empty(no) }.

/* Main predicate running parser

Example usage:
?- parse(_, `def main(A) = B`, Program).
*/

parse(_Path, Codes, Program) :-
    phrase(lexer(TokenList), Codes),
    phrase(program(Program), TokenList).
