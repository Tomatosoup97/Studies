
append([], X, X).
append([H|T], X, [H|S]) :-
    append(T, X, S).

head(H, [H|_]).

last(H, [H]).
last(H, [_|T]) :-
    last(H, T).

tail(T, [_|T]).

init([_], []).
init([H|LT], [H|TT]) :-
    init(LT, TT).

prefix([], _).
prefix([H|PT], [H|LT]) :-
    prefix(PT, LT).

suffix(S, S).
suffix([_|T], S) :-
    suffix(T, S).
