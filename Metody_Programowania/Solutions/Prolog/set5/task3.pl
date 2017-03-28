halve(X, L, R) :-
    halve(X, X, L, R).

halve(List, [], [], List) :- !.
halve(List, [_], [], List) :- !.
halve([Head|Tail], [_, _|AccT], [Head|LeftTail], R) :-
    halve(Tail, AccT, LeftTail, R).
