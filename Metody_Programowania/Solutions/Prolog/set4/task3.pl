allocate([_]).
allocate([_|X]) :-
    allocate(X).

bin_([]).
bin_([H|T]) :-
    (H = 0;
     H = 1),
    bin_(T).

bin([0]).
bin([1|T]) :-
    allocate(T),
    bin_(T).


rbin_([1]).
rbin_([H|T]) :-
    rbin_(T),
    (H = 0;
     H = 1).

rbin([0]).
rbin(X) :-
    allocate(X),
    rbin_(X).
