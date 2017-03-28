split([], [[]]) :- !.
split([X], [[X]]) :- !.
split([H|Tail], [[H]|SplittedTail]) :-
    split(Tail, SplittedTail).


merge_sort(List, SortedList) :-
    split(List, Splitted),
    merge_sort_(Splitted, SortedList).

merge_sort_([[]], []) :- !.
merge_sort_([X], X) :- !.
merge_sort_(Splitted, SortedList) :-
    merge_pairs(Splitted, MergedPairs),
    merge_sort_(MergedPairs, SortedList).


merge_pairs([], []) :- !.
merge_pairs([X], [X]) :- !.
merge_pairs([H1, H2|Tail], [Pair|Pairs]) :-
    merge(H1, H2, Pair),
    merge_pairs(Tail, Pairs).
