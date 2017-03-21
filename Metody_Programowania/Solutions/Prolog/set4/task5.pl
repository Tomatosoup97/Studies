flatten(leaf, []).
flatten(node, []).
flatten(X, [X]) :- number(X).
flatten(node(X, Y, Z), T) :-
    flatten(X, XF),
    flatten(Z, ZF),
    append(XF, [Y|ZF], T).


insert(leaf, Elem, Elem) :- !.
insert(Num, Elem, Dest) :-
    number(Num),
    DestR = node(leaf, Num, leaf),
    insert(DestR, Elem, Dest), !.

insert(node(X, Y, Z), Elem, Dest) :-
    Elem >= Y, !,
    insert(Z, Elem, DestR),
    Dest = node(X, Y, DestR).

insert(node(X, Y, Z), Elem, Dest) :-
    insert(X, Elem, DestR),
    Dest = node(DestR, Y, Z).


insertAll(Tree, [], Tree) :- !.
insertAll(SrcTree, [H|T], DestTree) :-
    insert(SrcTree, H, TreeR),
    insertAll(TreeR, T, DestTree).

insertAll(List, Tree) :-
    insertAll(leaf, List, Tree).

treesort(List, Sorted) :-
    insertAll(List, Tree),
    flatten(Tree, Sorted).
