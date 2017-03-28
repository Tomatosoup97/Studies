n_elements(List, 0, [], List) :- !.
n_elements([H|Tail], 1, [H], Tail) :- !.
n_elements([H|Tail], N, [H|Elements], Rest) :-
    NR is N - 1,
    n_elements(Tail, NR, Elements, Rest).


merge_sort(List, 0, List) :- !.
merge_sort([H|_], 1, [H]) :- !.
merge_sort(List, N, SortedList) :-
    N1 is N // 2,
    N2 is N - N1,
    n_elements(List, N1, ListN1, ListN2),
    merge_sort(ListN1, N1, SortedList1),
    merge_sort(ListN2, N2, SortedList2),
    merge(SortedList1, SortedList2, SortedList).
