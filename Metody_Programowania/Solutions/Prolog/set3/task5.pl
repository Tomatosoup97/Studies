insert([], X, [X]).
insert([H|T], Elem, Res) :-
    Elem >= H,
    insert(T, Elem, ResR), !,
    append([H], ResR, Res).

insert([H|T], Elem, Res) :-
    Elem < H,
    Res = [Elem, H|T].

ins_sort([], []).
ins_sort([H|T], Res) :-
    ins_sort(T, ResR),
    insert(ResR, H, Res).
