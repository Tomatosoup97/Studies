male(adam).
male(john).
male(joshua).
male(mark).
male(david).
female(eve).
female(helen).
female(ivonne).
female(anna).

parent(john, joshua).
parent(helen, joshua).

parent(ivonne, david).
parent(mark, david).

parent(adam, ivonne).
parent(eve, ivonne).

parent(adam, helen).
parent(eve, helen).

parent(adam, anna).
parent(eve, anna).


sibling(X, Y) :-
    parent(PARENT, X),
    parent(PARENT, Y),
    (X \== Y).

sister(X, Y) :-
    sibling(X, Y),
    female(X),
    (X \== Y).

grandson(S, G) :-
    parent(G, P),
    parent(P, S).

cousin(X, Y) :-
    male(X),
    parent(P1, X),
    parent(P2, Y),
    sibling(P1, P2),
    (X \== Y).

descendant(X, Y) :-
    parent(Y, X);
    (parent(Y, Z), descendant(X, Z)).

descendant_v2(X, Y) :-
    parent(Y, X).

descendant_v2(X, Y) :-
    parent(Y, Z),
    descendant(X, Z)).

is_mother(X) :-
    parent(X, _),
    female(X).

is_father(X) :-
    parent(X, _),
    male(X).
