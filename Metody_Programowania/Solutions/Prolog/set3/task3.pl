
factorial(2, 2).
factorial(N, M) :-
    NR is N - 1,
    factorial(NR, MR),
    M is MR * N.

concat_number(Digits, Num) :-
    fail.

%% decimal(0, [0]).
%% decimal(Num, [DH|DT]) :-
%%     DH = mod(Num, 10),
%%     NumR = Num * 10,
%%     decimal(NumR, DT).

