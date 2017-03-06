
select(H, [H|T], T).
select(X, [H|T], [H|S]) :-
    select(X, T, S).

append([], X, X).
append([H|T], X, [H|S]) :-
    append(T, X, S).

isDoubled(X, Y) :-
    append(X, X, Y).

% 1:
% ?- isDoubled(X, Y).

% 2:
% ?- select(X, [a, b, c, d], [a, c, d]).

% 3:
% ?- append([a, b, c], X, [a, b, c, d, e]).
