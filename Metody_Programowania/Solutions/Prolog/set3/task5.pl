%% Not working yet
insert([], X, [X]).
insert([H1, H2|T], Elem, Res) :-
    Elem =< H1,
    Elem >= H2,
    Res = [H1, Elem, H2|T].
insert([H|T], Elem, Res) :-
    Elem > H,
    insert(T, Elem, Res).
