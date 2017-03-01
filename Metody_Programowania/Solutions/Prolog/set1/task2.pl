%% 1

notHappy(ANIMAL) :-
    dragon(ANIMAL),
    livesInZoo(ANIMAL).

notDragon(ANIMAL) :-
    animal(ANIMAL),
    happy(ANIMAL),
    livesInZoo(ANIMAL).

%% 2, 3

happy(ANIMAL) :-
    human(PERSON),
    kind(PERSON),
    animal(ANIMAL),
    comeAcross(ANIMAL, PERSON).

kind(PERSON) :-
    human(PERSON),
    visitZoo(PERSON).


%% 4

comeAcross(ANIMAL, PERSON) :-
    human(PERSON),
    visitZoo(PERSON),
    animal(ANIMAL),
    livesInZoo(ANIMAL).

%% 5

animal(X) :- smok(X).

human(fowler).
visitZoo(fowler).
