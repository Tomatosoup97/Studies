perm([], []).
perm([LH|LT], P) :-
    perm(LT, PT),
    select(LH, P, PT).
