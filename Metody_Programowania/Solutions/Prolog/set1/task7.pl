is_next_to(1,2).
is_next_to(2,3).
is_next_to(3,4).
is_next_to(4,5).

neighbour(H1, H2) :-
    is_next_to(H1, H2);
    is_next_to(H2, H1).

% house(location, man, animal, cigarettes color, beverage)

house(1, norwegian, _, _, _, _).

% 15 norwegian lives next to bluehouse

house(HP1, norwegian, _, _, _, _) :-
    house(HP2, _, _, _, bluehouse, _),
    neighbour(HP1, HP2).

house(3, _, _, _, _, milk).

% 6 greenhouse is next to the whitehouse

house(HP1, _, _, _, greenhouse, _) :-
    house(HP2, _, _, _, whitehouse, _),
    neighbour(HP1, HP2).

house(_, _, _, _, greenhouse, coffee).
house(_, english, _, _, redhouse, _).
house(_, _, _, kool, yellowhouse, _).

% 12 In the house next to the horse owner, they smoke kool

house(HP1, _, horse, _, _, _) :-
    house(HP2, _, _, kool, _, _),
    neighbour(HP1, HP2).

house(_, ukrainian, _, _, _, tea).
house(_, _, _, luckystrike, _, juice).

% 11 chesterfield smoker is neighbour of fox owner

house(HP1, _, _, chesterfield, _, _) :-
    house(HP2, _, fox, _, _, _),
    neighbour(HP1, HP2).

house(_, japanese, _, kent, _, _).

house(_, _, snake, winston, _, _).

house(_, spanish, dog, _, _, _).

house(_, _, elephant, _, _, _).
house(_, _, _, _, _, vodka).

solution(ELEPHAT_OWNER, VODKA_DRINKER) :-
    house(_, ELEPHAT_OWNER, elephant, _, _, _),
    house(_, VODKA_DRINKER, _, _, _, vodka).

% ?- solution(ELEPHAT_OWNER, VODKA_DRINKER).
