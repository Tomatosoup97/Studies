:- module(tests, [tests/3]).

% Test lexer
% ---------------------

tests(wildcard, input("def main(_) = 1"), program([def(main, wildcard(no), num(no, 1))])).
tests(and, input("def main(A) = A & B"),
      program([
        def(main, var(no, 'A'), op(no, &, var(no, 'A'), var(no, 'B')) )
      ])).

% Variables

tests(variable_starting_underscore, input("def main(_A) = 1"),
      program([def(main, var(no, '_A'), num(no, 1))])).

tests(variable_inside_underscore, input("def main(A_A) = 1"),
      program([def(main, var(no, 'A_A'), num(no, 1))])).

tests(variable, input("def main(A) = 1"),
      program([def(main, var(no, 'A'), num(no, 1))])).

tests(variable_with_keyword, input("def main(defA) = 1"),
      program([def(main, var(no, 'defA'), num(no, 1) )])).

tests(variable_with_keywords, input("def main(def_deflet_in_else) = 1"),
      program([def(main, var(no, 'def_deflet_in_else'), num(no, 1) )])).

% Whitspaces

% Should result to same program, no matter of the whitspaces
tests(white_spaces1, input("def main(A     ) = 1 "),
      program([def(main, var(no, 'A'), num(no, 1) )])).

tests(white_spaces2, input("def    main(A     ) =    1"),
      program([def(main, var(no, 'A'), num(no, 1) )])).

tests(white_spaces3_newline, input("def main(A) \n = \n\n 1"),
      program([def(main, var(no, 'A'), num(no, 1) )])).

% Comments

tests(comment, input("def main(A) = (* Comment *) 1 "),
      program([def(main, var(no, 'A'), num(no, 1) )])).

tests(comment_with_code, input("def main(A) = (* A ^ B *) 1 "),
      program([def(main, var(no, 'A'), num(no, 1) )])).

tests(nesting_comments,
      input("def main(A) = (* Comment (* Nested one *) *) 1 "),
      program([def(main, var(no, 'A'), num(no, 1) )])).

tests(comment_all, input("(* def main(A) = Comment  1 *)"), yes).

tests(comment_invalid1, input("def main(A) = (* (* *) 1 "), no).

tests(comment_invalid2, input("def main(A) = (* *) *) 1 "), no).

tests(comment_invalid3, input("def main(A) = (* (* *) 1 "), no).

tests(comment_invalid4, input("(*(**) def main(A) =  1 "), program([])).


% Should not confuse <= and < :
tests(lte, input("def main(A) = A <= B"),
      program([
        def(main, var(no, 'A'), op(no, <=, var(no, 'A'), var(no, 'B')) )
      ])).

tests(lt, input("def main(A) = A < B"),
      program([
        def(main, var(no, 'A'), op(no, <, var(no, 'A'), var(no, 'B')) )
      ])).

% Invalid tokens

tests(invalid_token1, input("def main(A) = {{{{"), no).
tests(invalid_token2, input("def main({}{}{}) = 1"), no).


% Test parser
% ---------------------

tests(invalid, input("def main()"), no).

tests(basic_parser_bug, input("def main(_) = def"), no).
tests(invent_keyword, input("async def main(_) = A"), no).
tests(no_def_name, input("def (_) = A"), no).

tests(many_definitions,
      input("def first(A) = 1 \n def second(B) = 2 \n def third(_) = A"),
      program([
        def(first, var(no, 'A'), num(no, 1)),
        def(second, var(no, 'B'), num(no, 2)),
        def(third, wildcard(no), var(no, 'A'))
      ])).


tests(if_else, input("def main(A) = if A then B else C"), yes).
tests(if_else_invalid_tokens, input("def main(A) = if ~ then B else C"), no).

tests(let_in, input("def main(A) = let _ = A in B"), yes).
tests(let_in_invalid, input("def main(A) = let # = A in B"), no).

tests(unary_op, input("def main(A) = ~A"), yes).
tests(spam_unary_op, input("def main(A) = ~~~~~~A"), yes).
tests(unary_and_binary_op_invalid, input("def main(A) = ~A & B"), yes).

tests(bitsel, input("def main(A) = A1 [ A2 ]"), yes).
tests(bitsel2, input("def main(A) = A1 [ A2 .. A3]"), yes).
tests(bitsel2_invalid, input("def main(A) = A1 [ A2 .. .. .. A3]"), no).

tests(operators_associativity, input("def main(A) = A - B * C"),
      program(
        [def(main, var(no, 'A'), op(no, -, var(no, 'A'), op(no, *, var(no, 'B'), var(no, 'C'))))]
      )).

% Position

%% tests(position_basic, input("def main(_) = 1"),
%%   program([def(main, wildcard(file(test, 1, 10, 9, 1)), num(no, 1))])).

%% tests(position_newline, input("def main(_) = \nA"),
%%   program([def(main, wildcard(file(test, 1, 10, 9, 1)), num(no, 1))])).

%% tests(position_complex, input("def main(_) = let _ =\nA in B"),
%%   program([def(
%%     main, wildcard(file(test, 1, 10, 9, 1)),
%%     let(file(test, 1, 15, 14, 3),
%%         wildcard(test, 19, 18, 1),
%%         var(file(test, 2, 1, 21, 1), 'A'),
%%         var(file(test, 2, 6, 26, 1), 'B')
%%     )
%%   )])).

% From file
tests(adder, file('adder.hdml'), yes).
