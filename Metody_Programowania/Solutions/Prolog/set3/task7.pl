perm(X, Y) :- var(X), !, perm(Y, X).
perm([], []).
perm([LH|LT], P) :-
    perm(LT, PT),
    select(LH, P, PT).


perm(X, Y) :- var(X), !, perm(Y, X).
perm([], []).
perm(L, [PH|PT]) :-
    select(PH, L, LT),
    perm(LT, PT).


inse([],Elem,[Elem]).
inse([H|T],Elem,[Elem,H|T]):-
    Elem =< H,
    !.
inse([H|T],Elem,[H|Left]):-
    inse(T,Elem,Left).