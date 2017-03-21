connection(wroclaw, warszawa).
connection(wroclaw, krakow).
connection(wroclaw, szczecin).
connection(szczecin, lublin).
connection(szczecin, gniezno).
connection(warszawa, katowice).
connection(gniezno, gliwice).
connection(lublin, gliwice).
connection(gliwice, lublin).
connection(gliwice, katowice).


trip(City1, City2, Route, Route) :-
    connection(City1, City2), !.

trip(City1, City2, Acc, Route) :-
    connection(Inter, City2),
    \+ member(Inter, Acc),
    trip(City1, Inter, [Inter|Acc], Route).

trip(City1, City2, [City1|Route]) :-
    trip(City1, City2, [City2], Route).
