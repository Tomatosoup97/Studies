appn([List], List) :- !.
appn([List1, List2| ListsTail], Result) :-
    append(List1, List2, List3),
    appn([List3|ListsTail], Result).
