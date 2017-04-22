%% :- module(main, [parse/3]).

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
                     |      <expr op>
    <expr op>       ::=     <expr op> <binary_op> <expr op>
                     |      <unary_op> <expr op>
                     |      <simple_expr>
    <binary_op>     ::=     , | = | <> | < | > | <= | >= | ^ | | | + | - | & | * | / | % | @
    <unary_op>      ::=     - | # | ~
    <simple_expr>   ::=     ( <expr> )
                     |      <byte_choice>
                     |      <bytes_choice>
                     |      <atomic_expr>
    <byte_choice>   ::=     <simple_expr> [ <expr> ]
    <bytes_choice>  ::=     <simple_expr> [ <expr> .. <expr> ]
    <atomic_expr>   ::=     <variable>
                     |      <func_exec>
                     |      <float_literal>
                     |      <empty_vector>
                     |      <single_bit>
    <variable>      ::=     <identifier>
    <func_exec>     ::=     <identifier> ( <expr> )
    <empty_vector>  ::=     [ ]
    <single_bit>    ::=     [ <expr> ]
    <empty>         ::=

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
     ; "|",     !, { Token = tokenOr }
     ; "^",     !, { Token = tokenExp }
     ; "+",     !, { Token = tokenPlus }
     ; "-",     !, { Token = tokenMinus }
     ; "@",     !, { Token = tokenAt }
     ; "=",     !, { Token = tokenAssign }
     ; "<>",    !, { Token = tokenNe }
     ; "<",     !, { Token = tokenLt }
     ; ">",     !, { Token = tokenGt }
     ; "<=",    !, { Token = tokenLte }
     ; ">=",    !, { Token = tokenGte }
     ; ",",     !, { Token = tokenComma }
     ; "..",    !, { Token = tokenDoubleDot }
     ; "(",     !, { Token = tokenLParen }
     ; ")",     !, { Token = tokenRParen }
     ; "[",     !, { Token = tokenLBracket }
     ; "]",     !, { Token = tokenRBracket }
     ; "#",     !, { Token = tokenHash }
     ; "~",     !, { Token = tokenTilde }
     ; digit(D1),
         number(D1, N1),
         ".", !,
         digit(D2),
         number(D2, N2),
         { Token = tokenFloat(N1, N2) }
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
                                ("_", tokenUnderscore)]),
           !
         ; Token = tokenVar(Id)
         }
     ; [_], { Token = tokenUnknown }
     ),
     !,
     { Tokens = [Token | TokenList] },
     lexer(TokenList)
    ; [],
        { Tokens = [] }
    ).


white_space -->
    [Char], { code_type(Char, space) }, !, white_space.
white_space -->
    [].


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
* SYNTAX ANALYSIS -- PARSER
* BNF grammar listed at the beggining of the file
*
* */


% OPERATORS

%:- op(900, xfy, def).



% Parser

program(AST) -->
    definitions(AST).


% TODO: Not sure about this one
definitions(Defs) -->
    definition(DefHead), !,
    definitions(DefTail),
    { Defs = [DefHead | DefTail] }.

definitions(Defs) --> empty(Defs).


definition(Def) -->
    [tokenDef], identifier(Id),
    [tokenLParen], pattern(Pattern), [tokenRParen],
    [tokenAssign], expr(Expr),
    { Def = def(Id, Pattern, Expr) }.


pattern(Pattern) -->
    (   [tokenUnderscore], !,
        { Pattern = wildcard(no) }

    ;   variable(Var), !,
        { Pattern = Var }

    ;   [tokenLParen], !, pattern(Pattern), [tokenRParen]

    ;   pattern(Pattern1), [tokenComma], pattern(Pattern2),
        { Pattern = pair(no, Pattern1, Pattern2) }
    ).


expr(Expr) -->
    (   [tokenIf], expr(Expr1),
        [tokenThen], expr(Expr2),
        [tokenElse], expr(Expr3),
        { Expr = if(no, Expr1, Expr2, Expr3) }, !

    ;   [tokenLet], pattern(Pattern),
        [tokenAssign], expr(Expr1),
        [tokenIn], expr(Expr2),
        { Expr = let(no, Pattern, Expr1, Expr2) }, !

    ;   expr_op(ExprOp),
        { Expr = ExprOp }
    ).


% Left-side recursion !!
expr_op(Op) -->
    (   expr_op(ExprOp1), !, binary_op(BinaryOp), expr_op(ExprOp2),
        { Op = op(no, BinaryOp, ExprOp1, ExprOp2) }
    
    ;   unary_op(UnaryOp), !, expr_op(ExprOp),
        { Op = op(no, UnaryOp, ExprOp) }
    
    ;   simple_expr(SimpleExpr),
        { Op = SimpleExpr }
    ).


binary_op(',') --> [tokenComma], !.
binary_op(=) --> [tokenAssign], !.
binary_op(<>) --> [tokenNe], !.
binary_op(<) --> [tokenLt], !.
binary_op(>) --> [tokenGt], !.
binary_op(<=) --> [tokenLte], !.
binary_op(>=) --> [tokenGte], !.
binary_op(^) --> [tokenExp], !.
binary_op('|') --> [tokenOr], !.
binary_op(+) --> [tokenPlus], !.
binary_op(-) --> [tokenMinus], !.
binary_op(&) --> [tokenAnd], !.
binary_op(*) --> [tokenMult], !.
binary_op(/) --> [tokenDiv], !.
binary_op('%') --> [tokenMod], !.
binary_op(@) --> [tokenAt], !.


unary_op(-) --> [tokenMinus], !.
unary_op(#) --> [tokenHash], !.
unary_op(~) --> [tokenTilde], !.


simple_expr(Expr) -->
    (   [tokenLParen], !, expr(Expr), [tokenRParen]
    ;   byte_choice(Expr), !
    ;   bytes_choice(Expr), !
    ;   atomic_expr(Expr)
    ).


byte_choice(Choice) -->
    simple_expr(SimpleExpr),
    [tokenLBracket],
    expr(Expr),
    [tokenRBracket],
    { Choice = bitsel(no, SimpleExpr, Expr) }.


bytes_choice(Choice) -->
    simple_expr(SimpleExpr),
    [tokenLBracket],
    expr(Expr1),
    [tokenDoubleDot],
    expr(Expr2),
    [tokenRBracket],
    { Choice = bitsel(no, SimpleExpr, Expr1, Expr2) }.


atomic_expr(Expr) -->
    (   variable(Expr), !
    ;   func_exec(Expr), !
    ;   float_literal(Expr), !
    ;   empty_vector(Expr), !
    ;   single_bit(Expr)
    ).


variable(ParsedVar) -->
    [tokenVar(Var)],
    { ParsedVar = var(no, Var) }.


func_exec(Func) -->
    identifier(Id),
    [tokenLParen],
    expr(Expr),
    [tokenRParen],
    { Func = call(no, Id, Expr) }.


empty_vector(Expr) -->
    [],
    { Expr = empty(no) }.


single_bit(Bit) -->
    [tokenLBracket],
    expr(Expr),
    [tokenRBracket],
    { Bit = bit(no, Expr) }.


empty(Expr) -->
    [],
    { Expr = empty(no) }.


parse(_Path, Codes, Program) :-
    phrase(lexer(TokenList), Codes),
    phrase(program(Program), TokenList).


