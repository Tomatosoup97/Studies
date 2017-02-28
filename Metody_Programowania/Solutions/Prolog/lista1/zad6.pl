connection(wroclaw, warszawa).
connection(wroclaw, krakow).
connection(wroclaw, szczecin).
connection(szczecin, lublin).
connection(szczecin, gniezno).
connection(warszawa, katowice).
connection(gniezno, gliwice).
connection(lublin, gliwice).


isConnectionOneTransfer(X, Y) :-
    connection(X, Z),
    connection(Z, Y).

isConnectionTwoTransfers(A, D) :-
    connection(A, B),
    connection(B, C),
    connection(C, D).

isConnectionMaxTwoTransfers(X, Y) :-
    connection(X, Y);
    isConnectionOneTransfer(X, Y);
    isConnectionTwoTransfers(X, Y).

isConnection(X, Y) :-
    connection(X, Y);
    (isConnection(X, Z), isConnection(Z, Y)).

isConnection_v2(X, Y) :-
    connection(X, Y).

isConnection_v2(X, Y) :-
    isConnection(X, Z),
    isConnection(Z, Y).


%% ?- connection(wroclaw, lublin).
%% ?- connection(wroclaw, X).
%% ?- isConnectionOneTransfer(X, gliwice).
%% ?- isConnectionMaxTwoTransfers(X, gliwice).
