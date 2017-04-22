:- module(tests, [resolve_tests/5, prove_tests/4]).

:- op(200, fx, ~).
:- op(500, xfy, v).

% Resolve tests
% ----------------------------------------------------------------------

resolve_tests(empty, q, q, ~q, []).
resolve_tests(simple_test, q, p v q, ~q v r, p v r).
resolve_tests(basic, q, p v q, ~q, p).
resolve_tests(medium, p, p v q v ~r v p v q v p, ~r v w v ~p v ~p, q v ~r v w).
resolve_tests(duplicates, p, p v p v p v p v p v p v q, ~p v ~p v ~p v ~p v ~p v w, q v w).
resolve_tests(many_vars, p, p v p1 v p2 v p3 v p4 v p5 v p6 v p7 v p8 v p9 v p10, ~p v ~p1 v ~p2 v ~p3 v ~p4 v ~p5 v ~p6 v ~p7 v ~p8, p1 v p2 v p3 v p4 v p5 v p6 v p7 v p8 v p9 v p10 v ~p1 v ~p2 v ~p3 v ~p4 v ~p5 v ~p6 v ~p7 v ~p8).

resolve_tests(pusta, p, p, ~p, []).
resolve_tests(krotkie_1, p, p v q v r, ~p v q v r, q v r).
resolve_tests(krotkie_2, q, p v q, ~q v r, p v r).
resolve_tests(krotkie_3, p, p v q, ~p, q).
resolve_tests(krotkie_4, r, p v q v r, ~r, p v q).
resolve_tests(krotkie_pozneg_1, p, p v q v r, ~p v ~q v ~r, q v r v ~q v ~r).
resolve_tests(krotkie_pozneg_2, q, q v r, ~q v ~r v t, r v ~r v t).
resolve_tests(krotkie_pozneg_3, r, p v r, ~p v ~r, p v ~p).
resolve_tests(krotkie_pozneg_4, p, p v ~q v r, ~p v ~q v ~r, ~q v r v ~r).
resolve_tests(dlugie_1, p, p v q v ~r v s v ~t v w, ~p v ~t v w, q v ~r v s v ~t v w).
resolve_tests(dlugie_2, p, p v q v r v s v t v w v x v y v z, ~p, q v r v s v t v w v x v y v z).
resolve_tests(dlugie_3, p, p v q v r v s v t v w v x v y v z, ~p v a v b v c v d v e v f v g, q v r v s v t v w v x v y v z v a v b v c v d v e v f v g).
resolve_tests(dlugie_4, p, p v q v r v s v t, ~p v q v r v s v t, q v r v s v t).
resolve_tests(dlugie_pozneg_1, p, p v q v ~r v s v ~t v w, ~p v t v ~w, q v ~r v s v ~t v w v t v ~w).
resolve_tests(dlugie_pozneg_2, p, p v q v r v s v t v w v x v y v z, ~p v ~z, q v r v s v t v w v x v y v z v ~z).
resolve_tests(dlugie_pozneg_3, p, p v q v r v s v t v w v x v y v z, ~p v a v b v c v d v e v f v g v ~z, q v r v s v t v w v x v y v z v a v b v c v d v e v f v g v ~z).
resolve_tests(dlugie_pozneg_4, p, p v q v r v s v t, ~p v ~q v ~r v ~s v ~t, q v r v s v t v ~q v ~r v ~s v ~t).

resolve_tests(empty_duplicates, p, p v p v p, ~p v ~p v ~p, []).
resolve_tests(right_empty, p, p v a, ~p, a).
resolve_tests(left_empty, p, p, ~p v a, a).
resolve_tests(tautologies1, p, p v ~p, ~p v p, ~p v p).
resolve_tests(tautologies2, p, p, ~p v p, p).
resolve_tests(tautologies3, p, p v ~p, ~p, ~p).


% Prove tests
% ----------------------------------------------------------------------

% Validity

prove_tests(excluded_middle, validity, [p v ~p], sat).
prove_tests(one_literal, validity, [p], sat).
prove_tests(negated_literal, validity, [~p], sat).
prove_tests(two_same, validity, [p, p], sat).
prove_tests(two_negations, validity, [~p, ~p], sat).
prove_tests(two_true, validity, [p v q], sat).
prove_tests(doubled, validity, [p v q, ~p v ~q], sat).
prove_tests(three_var, validity, [p v q v r, ~r v ~q v ~p, ~q v r, ~r], sat).
prove_tests(fourVars, validity, [p v q v r v w, ~p, ~r, q], sat).
prove_tests(fiveVars, validity, [p v q v r v w, ~p, ~r, q, p v ~q v o, w v r], sat).
prove_tests(chaining, validity, [p, ~p v q, ~q v r, ~r v w, ~w v o, ~o v k, ~p1 v q1, ~q1 v r1, ~r1 v w1, ~w1 v o1, ~o1 v k1, ~p2 v q2, ~q2 v r2, ~r2 v w2, ~w2 v o2, ~o2 v k2, ~p3 v q3, ~q3 v r3, ~r3 v w3, ~w3 v o3, ~o3 v k3], sat).
prove_tests(many_vars, validity, [p v q v r, n v m v w, u v o v k, a v b v c, d v e v f, h v i v j v k], sat).
prove_tests(two_repeated_vars, validity ,[q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v q v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p v p], sat).
prove_tests(basic1, validity, [p v q, ~p, ~q], unsat).
prove_tests(basic2, validity, [p v q v ~r, ~p, ~q v p, r], unsat).
prove_tests(example, validity, [p v q v ~r, ~p v q, r v q, ~q, p], unsat).

prove_tests(malo_klauzul_malo_zmiennych_1, validity, [p], sat).
prove_tests(malo_klauzul_malo_zmiennych_2, validity, [~p], sat).
prove_tests(malo_klauzul_malo_zmiennych_3, validity, [p v p], sat).
prove_tests(malo_klauzul_malo_zmiennych_4, validity, [~p v ~p], sat).
prove_tests(malo_klauzul_malo_zmiennych_5, validity, [p v q], sat).
prove_tests(malo_klauzul_malo_zmiennych_6, validity, [p v ~q], sat).
prove_tests(malo_klauzul_malo_zmiennych_7, validity, [~p v ~q], sat).
prove_tests(malo_klauzul_duzo_zmiennych_1, validity, [p v q v r], sat).
prove_tests(malo_klauzul_duzo_zmiennych_2, validity, [p v q v ~r], sat).
prove_tests(malo_klauzul_duzo_zmiennych_3, validity, [p v ~q v ~r], sat).
prove_tests(malo_klauzul_duzo_zmiennych_4, validity, [~p v ~q v ~r], sat).
prove_tests(duzo_klauzul_malo_zmiennych_1, validity, [p, p], sat).
prove_tests(duzo_klauzul_malo_zmiennych_2, validity, [p, q], sat).
prove_tests(duzo_klauzul_malo_zmiennych_3, validity, [p, ~q], sat).
prove_tests(duzo_klauzul_malo_zmiennych_4, validity, [p v q, ~p v q, ~p v ~q], sat).
prove_tests(duzo_klauzul_duzo_zmiennych_1, validity, [~p v r, p v q v r, p v ~q v r], sat).
prove_tests(duzo_klauzul_duzo_zmiennych_2, validity, [p, p v r, r v q, p v ~r], sat).
prove_tests(duzo_klauzul_duzo_zmiennych_3, validity, [~q v r, ~r v p, p v q v r, ~r v ~q v ~p], sat).
prove_tests(duzo_klauzul_duzo_zmiennych_4, validity, [p, ~p v q, ~q v r, ~p v ~r v s], sat).
prove_tests(duzo_klauzul_duzo_zmiennych_5, validity, [p, q, r, s, u, w, ~y, z], sat).
prove_tests(pusta_pusta, validity, [[]], unsat).
prove_tests(negacja, validity, [p, ~p], unsat).
prove_tests(wieksza_negacja, validity, [p, ~p, q, r, s, y], unsat).
prove_tests(pusta, validity, [], sat).
prove_tests(wylaczony_srodek_val, validity, [p v ~p], sat).
prove_tests(wiekszy_wylaczony_srodek, validity, [p v ~p v q v r v s], sat).

prove_tests(excluded_middle, validity, [p v ~p], sat).
prove_tests(empty_set, validity, [], sat).
prove_tests(empty_clause, validity, [[]], unsat).
prove_tests(unsat1, validity, [p, ~p], unsat).
prove_tests(unsat2, validity, [p v q, p v ~p, []], unsat).
prove_tests(sat1, validity, [~p v q, ~q v r, ~r v s, ~s], sat).
