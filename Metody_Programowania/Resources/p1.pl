/*
Propositional Logic Semantic Tableaux
From Ben-Ari: Mathematical Logic for Computer Science (Appendix B)
( in /home/ta5/staff/alanw/PROLOG/semtab.pl )
*/
/* Define logical operators: */
:- op(650,xfy, #).
:- op(640,xfy, =>).
:- op(630,yfx, ˆ).
:- op(620,yfx, v).
:- op(610, fy, ˜).
/* Top-level algorithm */
semantic_tableau(F) :- T = t(_, _, [F]),
  extend_tableau(T),
  write_tableau(T,0).
extend_tableau(t(closed, empty, L)) :-
  check_closed(L).
%---------------------------------
extend_tableau(t(open, empty, L)) :-
  contains_only_literals(L).
extend_tableau(t(Left, empty, L)) :-
  alpha_rule(L,L1),
  Left = t(_,_,L1),
  extend_tableau(Left).
extend_tableau(t(Left, Right, L)) :-
  beta_rule(L,L1,L2),
  Left = t(_,_,L1),
  Right = t(_,_,L2),
  extend_tableau(Left),
  extend_tableau(Right).
/* tableau extension */
check_closed(L) :-
  mymember(F,L), mymember( ˜ F, L).
contains_only_literals([]).
contains_only_literals([F | Tail]) :-
  literal(F),
  contains_only_literals(Tail).
  literal(F) :- atom(F).
  literal(˜ F) :- atom(F).
  alpha_rule(L, [A1, A2 | Ltemp]) :-
  alpha_formula(A,A1,A2),
  mymember(A,L),
  delete(A,L, Ltemp).
  alpha_rule(L, [A1 | Ltemp]) :-
  A = ˜ ˜ A1,
  mymember(A,L),
  delete(A,L, Ltemp).
  beta_rule(L, [B1 | Ltemp], [B2 | Ltemp]) :-
  beta_formula(B,B1,B2),
  mymember(B,L),
  delete(B,L, Ltemp).
  alpha_formula(A1 ˆ A2, A1, A2).
  alpha_formula(˜ (A1 => A2), A1, ˜ A2).
  alpha_formula(˜ (A1 v A2), ˜ A1, ˜ A2).
  alpha_formula(˜ (A1 # A2), ˜ (A1 => A2), ˜( A2 => A1) ).
  beta_formula(A1 v A2, A1, A2).

  beta_formula(A1 => A2, ˜ A1, A2).
  beta_formula(˜ (A1 ˆ A2), ˜ A1, ˜ A2).
  beta_formula(A1 # A2, A1 => A2, A2 => A1).
  /* printing the tableau */
  write_formula_list([F]) :- write(F).
  write_formula_list([F | Tail]) :-
  write(F),
  write(’,’),
  write_formula_list(Tail).
  write_tableau(empty,_).
  write_tableau(closed,_) :-
  write( Closed).
  write_tableau(open,_) :-
  write( Open ).
  write_tableau(t(Left, Right, List), K) :-
  nl, tab(K), K1 is K+3,
  write_formula_list(List),
  write_tableau(Left,K1),
  write_tableau(Right,K1).

  /* standard list operations */
mymember(X, [X | _]).
mymember(X, [_ | Tail]) :- mymember(X,Tail).
delete(X, [X | Tail], Tail).
delete(X, [Head | Tail], [Head | Tail1]) :- delete(X, Tail, Tail1).
/* Example: from above
semantic_tableau( ((p ˆ q) ˆ ˜ q) ).
*/
