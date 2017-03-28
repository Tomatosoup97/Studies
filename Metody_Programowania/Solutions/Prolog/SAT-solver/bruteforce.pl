:- module(main, [solve/2]).

:- op(200, fx, ~).
:- op(500, xfy, v).

literal(L) :- atom(L).

solve([], []).

solve([L|CT], [(L, t)|ST]) :-
    literal(L),
    solve(CT, ST),
    \+ member((L, _), ST).

solve([L|CT], S) :-
    %% Don't add new (L, t) if already in S
    literal(L),
    solve(CT, S),
    member((L, t), S).

solve([~L|CT], [(L, f)|ST]) :-
    literal(L),
    solve(CT, ST),
    \+ member((L, _), ST).

solve([~L|CT], S) :-
    %% Don't add new (L, f) if already in S
    literal(L),
    solve(CT, S),
    member((L, f), S).

solve([A v B|T], S) :-
    solve([A|T], S);
    solve([B|T], S).
