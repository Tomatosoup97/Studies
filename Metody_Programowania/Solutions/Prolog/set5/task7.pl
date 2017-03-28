split([], _, [], []) :- !.
split([H|Tail], Med, [H|Small], Big) :-
    H < Med, !,
    split(Tail, Med, Small, Big).

split([H|Tail], Med, Small, [H|Big]) :-
    split(Tail, Med, Small, Big).


qsort([], []).
qsort(List, Sorted) :-
    qsort(List, [], Sorted).

qsort([], Sorted, Sorted).
qsort([H|Tail], Acc, Sorted) :-
    split(Tail, H, Small, Big),
    qsort(Big, Acc, AccSorted),
    qsort(Small, [H|AccSorted], Sorted).
