even([]).
even([_|[_|T2]]) :-
    even(T2).

truncLastAndFirst(L, R) :- 
    init(L, WITHOUT_LAST),
    tail(R, WITHOUT_LAST).

palindrom([]).
palindrom([X]).
palindrom(X) :-
    head(EL, X),
    last(EL, X),
    truncLastAndFirst(X, R),
    palindrom(R).

singleton_v1([_|[]]).
% Ensure in v2 that H is not a nested list
singleton_v2([H|[]]) :-
    [_|_] \= H.
