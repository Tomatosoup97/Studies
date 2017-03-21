length_([], N, N).
length_([_|T], A, N) :-
    A \== N,
    A1 is A + 1,
    length_(T, A1, N).
length_(L, N) :-
    length_(L, 0, N).
