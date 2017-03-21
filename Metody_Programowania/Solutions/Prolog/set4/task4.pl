
mirror(leaf, leaf).

% These 4 clauses can be merged into one
mirror(node(X, Y, Z), M) :-
    X = node(_, _, _),
    mirror(X, XM),
    Z = node(_, _, _),
    mirror(Z, ZM), !,
    M = node(ZM, Y, XM).

mirror(node(X, Y, Z), M) :-
    X = node(_, _, _),
    mirror(X, XM), !,
    M = node(Z, Y, XM).

mirror(node(X, Y, Z), M) :-
    Z = node(_, _, _),
    mirror(Z, ZM), !,
    M = node(ZM, Y, X).

mirror(D, M) :-
    D = node(X, Y, Z),
    M = node(Z, Y, X).

% Easy has to be improved to work with nested nodes
flattenEasy(X, Y) :-
    X =.. Y.

flatten(X, [X]) :- (atom(X); number(X)), !.
flatten(node(X, Y, Z), [node|T]) :-
    flatten(X, XF),
    flatten(Z, ZF),
    append(XF, [Y|ZF], T).
