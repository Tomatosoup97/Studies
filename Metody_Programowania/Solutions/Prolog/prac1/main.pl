:- module(mateusz_urbanczyk, [solve/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).

literal(L) :- atom(L).

solve([], []).

solve([L|CT], [(L, t)|ST]) :-
    literal(L),
    solve(CT, ST),
    \+ member((L, f), ST).

solve([~L|CT], [(L, f)|ST]) :-
    literal(L),
    solve(CT, ST),
    \+ member((L, t), ST).

solve([A v B|T], S) :-
    solve([A|T], S);
    solve([B|T], S).
