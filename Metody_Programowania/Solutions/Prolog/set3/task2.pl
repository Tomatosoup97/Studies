
filter([], []).
filter([H|LT], [H|PT]) :-
    H >= 0,
    filter(LT, PT),
    !.
filter([H|T], P) :-
    H < 0,
    filter(T, P).


%% Check twice cut location
count(_, [], 0).
count(Elem, [Elem|LT], Count) :-
    count(Elem, LT, CountR),
    Count is CountR + 1,
    !.
count(Elem, [_|LT], Count) :-
    count(Elem, LT, Count).


exp(_, 0, 1).
exp(0, _, 0).
exp(Base, Exp, Res) :-
    ExpR is Exp - 1,
    exp(Base, ExpR, ResR),
    Res is ResR * Base.
