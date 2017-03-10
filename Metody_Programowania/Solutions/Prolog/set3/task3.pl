factorial(0, 1) :- !.
factorial(N, M) :-
    NR is N - 1,
    factorial(NR, MR),
    M is MR * N.


concat_number([X], X).
concat_number([Dig|Digits], Num) :-
    concat_number(Digits, NumR),
    Num is NumR * 10 + Dig.


decimal(0, []) :- !.
decimal(Num, [H|T]) :-
    Bit is Num mod 10,
    H = Bit,
    NumR is Num // 10,
    decimal(NumR, T).
