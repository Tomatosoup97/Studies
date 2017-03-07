
sublist_v1(_, []).
sublist_v1(L, S) :-
    append(_, S, L);
    append(S, _, L);

    append(_, S, R),
    append(R, _, L).


sublist(_, []).
sublist([H|LT], [H|ST]) :-
    sublist(LT, ST).

sublist(L, S) :-
    [_|LT] = L,
    sublist(LT, S).
