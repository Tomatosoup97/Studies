bird(_) :- fail.
earthworm(_) :- fail.
fish(_) :- fail.

cat(my_cat).

likes(X, Y) :- bird(X), earthworm(Y).
likes(X, Y) :- cat(X), fish(Y).

likes(X, Y) :- friend(X, Y).
likes(Y, X) :- friend(X, Y).

friend(my_cat, me).

eats(my_cat, X) :- likes(my_cat, X).

%% ?- eats(my_cat, X).
