:- module(mateusz_urbanczyk_tests, [tests/5]).

:- op(200, fx, ~).
:- op(500, xfy, v).


%% Basic tests

tests(one_literal, validity, [p], 500, solution([(p, t)])).
tests(negated_literal, validity, [~p], 500, solution([(p, f)])).

tests(two_same, validity, [r, r], 500, solution([(r, t)])).
tests(two_same_c, validity, [r, r], 500, count(1)).

tests(two_true, validity, [p v q], 500, solution([(p, t)])).
tests(two_true_c, validity, [p v q], 500, count(2)).

tests(excluded_middle, validity, [~p v p], 500, solution([(p, t)])).

tests(two_true_two_false, validity, [p v q, ~p v ~q], 500,
                          solution([(p, t), (q, f)])).
tests(two_true_two_false_c, validity, [p v q, ~p v ~q], 500, count(2)).

%% More variables

tests(basic1, validity, [p v ~q v r, r], 500, solution([(p, t), (r, t)])).
tests(basic1_c, validity, [p v ~q v r, r], 500, count(2)).


tests(basic2, validity, [p v q v r, ~p v ~q v ~r], 500,
              solution([(r, x), (p, t),  (q, f)])).
tests(basic2_c, validity, [p v q v r, ~p v ~q v ~r], 500, count(6)).


tests(more1, validity, [p v q v r v w, ~p, ~r, q], 500,
             solution([(w, t), (p, f), (r, f), (q, t)])).
tests(more1_c, validity, [p v q v r v w, ~p, ~r, q], 500, count(1)).
