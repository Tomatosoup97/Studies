:- module(mateusz_urbanczyk_tests, [tests/5]).

:- op(200, fx, ~).
:- op(500, xfy, v).


%% The most basic cases

tests(one_literal, validity, [p], 500, solution([(p, t)])).
tests(negated_literal, validity, [~p], 500, solution([(p, f)])).

tests(two_same, validity, [p, p], 500, solution([(p, t)])).
tests(two_same_c, validity, [p, p], 500, count(1)).

tests(two_negations, validity, [~p, ~p], 500, solution([(p, f)])).
tests(two_negations_c, validity, [~p, ~p], 500, count(1)).

%% Basic with alternatives

tests(two_true, validity, [p v q], 500, solution([(p, t)])).
tests(two_true_c, validity, [p v q], 500, count(2)).

tests(excluded_middle, validity, [~p v p], 500, solution([(p, t)])).

tests(two_true_two_false, validity, [p v q, ~p v ~q], 500,
                          solution([(p, t), (q, f)])).
tests(two_true_two_false_c, validity, [p v q, ~p v ~q], 500, count(2)).

%% Three-variables

tests(basic1, validity, [p v ~q v r, r], 500, solution([(p, t), (r, t)])).
tests(basic1_c, validity, [p v ~q v r, r], 500, count(3)).

tests(basic2, validity, [p v q v r, ~p v ~q v ~r], 500,
              solution([(p, t),  (q, f)])).
tests(basic2_c, validity, [p v q v r, ~p v ~q v ~r], 500, count(6)).

%% Multiple variables

tests(fourVars, validity, [p v q v r v w, ~p, ~r, q], 1000,
                solution([(w, t), (p, f), (r, f), (q, t)])).
tests(fourVars_c, validity, [p v q v r v w, ~p, ~r, q], 1000, count(2)).

tests(fiveVars, validity, [p v q v r v w, ~p, ~r, q, p v ~q v o, w v r], 1500,
                solution([(p, f),  (r, f),  (q, t),  (o, t),  (w, t)])).
tests(fiveVars_c, validity,
      [p v q v r v w, ~p, ~r, q, p v ~q v o, w v r], 1500, count(1)).

%% Chaining

tests(chaining, validity,
      [p, ~p v q, ~q v r, ~r v w, ~w v o, ~o v k], 2000,
      solution([(p, t),  (q, t),  (r, t),  (w, t),  (o, t),  (k, t)])).
tests(chaining_c, validity,
      [p, ~p v q, ~q v r, ~r v w, ~w v o, ~o v k], 2000, count(1)).


%% Multiple solutions

% [3 vars, 3 vars] 3*3 = 9
tests(many_solutions, validity,
      [p v q v r, w v u v o], 1000, count(9)).

% [5 vars, 5 vars] 5*5 = 25
tests(even_more_solutions, validity,
      [p v q v r v n v m, w v u v o v k v s], 1500, count(25)).

% [3 vars, 3 vars, 3 vars] 3*3*3 = 27
tests(cubic_solutions, validity,
      [p v q v r, n v m v w, u v o v k], 1500, count(27)).

%% tests(neutral_value, validity, [p v r, r], 500, solution([(r, t), (p, x)])).
