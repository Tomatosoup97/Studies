flatten(List, FlatList) :-
    flatten(List, [], ReversedFlat),
    reverse(ReversedFlat, FlatList).

flatten([], Flat, Flat) :- !.
flatten([Head|Tail], Acc, Flat) :-
    number(Head), !,
    flatten(Tail, [Head|Acc], Flat).

flatten([Head|Tail], Acc, Flat) :-
    flatten(Head, Acc, AccFlat),
    flatten(Tail, AccFlat, Flat).
