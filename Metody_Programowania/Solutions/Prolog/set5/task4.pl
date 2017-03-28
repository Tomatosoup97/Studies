merge([], X, X) :- !.
merge(X, [], X) :- !.
merge([H1|T1], [H2|T2], [H1|ResultTail]) :-
    H1 < H2, !,
    merge(T1, [H2|T2], ResultTail).

merge([H1|T1], [H2|T2], [H2|ResultTail]) :-
    merge([H1|T1], T2, ResultTail).


merge_sort([], []) :- !.
merge_sort([X], [X]) :- !.
merge_sort(List, SortedList) :-
    halve(List, LeftHalf, RightHalf),
    merge_sort(LeftHalf, SortedLeftHalf),
    merge_sort(RightHalf, SortedRightHalf),
    merge(SortedLeftHalf, SortedRightHalf, SortedList).
