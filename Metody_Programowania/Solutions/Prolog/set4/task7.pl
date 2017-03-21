list([]).
list([_|T]) :- list(T).

revall([], A, A) :- !.
revall([H|T], A, Y) :-
    list(H),
    revall(H, HR), !,
    revall(T, [HR|A], Y).

revall([H|T], A, Y) :-
    revall(T, [H|A], Y).

revall(X, Y) :-
    revall(X, [], Y).
