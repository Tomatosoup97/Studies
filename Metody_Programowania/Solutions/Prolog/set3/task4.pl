%% Can be optimized
select_min([X], X).
select_min([H1, H2|T], Min) :-
    H1 =< H2,
    select_min([H1|T], Min).

select_min([H1, H2|T], Min) :-
    H1 > H2,
    select_min([H2|T], Min).

select_min(NumList, Min, Rest) :-
    select_min(NumList, Min),
    select(Min, NumList, Rest).


sel_sort([], []).
sel_sort(List, [H|T]) :-
    select_min(List, H, Rest),
    sel_sort(Rest, T).
