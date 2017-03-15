%% These 1st versions are slow because you need to reverse the list afterwards

concat_number1([X], X).
concat_number1([Dig|Digits], Num) :-
    concat_number(Digits, NumR),
    Num is NumR * 10 + Dig.

decimal1_(0, []) :- !.
decimal1_(Num, [H|T]) :-
    Bit is Num mod 10,
    H = Bit,
    NumR is Num // 10,
    decimal1_(NumR, T).

decimal1(Num, List) :-
    decimal1_(Num, ListR),
    reverse(ListR, List).

%% --

factorial(0, 1).
factorial(N, M) :-
    NR is N - 1,
    factorial(NR, MR),
    M is MR * N.


concat_number([], Res, Res).
concat_number([Dig|Digits], A, Res) :-
    RecA is A * 10 + Dig,
    concat_number(Digits, RecA, Res).

concat_number(Digits, Num) :-
    concat_number(Digits, 0, Num).


decimal(0, A, Res) :-
    !, Res = A.

decimal(Num, A, Res) :-
    Bit is Num mod 10,
    NumR is Num // 10,
    decimal(NumR, [Bit|A], Res).

decimal(Num, Digits) :-
    decimal(Num, [], Digits).
