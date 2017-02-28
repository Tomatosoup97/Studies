is_next_to(1,2).
is_next_to(2,3).
is_next_to(3,4).
is_next_to(4,5).

neighbour(H1, H2) :-
    is_next_to(H1, H2);
    is_next_to(H2, H1).

% house(location, man, animal, cigarettes color, beverage)

house(_, english, _, _, redhouse, _).
house(_, spanish, dog, _, _, _).
house(_, _, _, _, greenhouse, coffee).
house(_, ukrainian, _, _, _, tea).

% greenhouse is next to the whitehouse

house(HP1, _, _, _, greenhouse, _) :-
    house(HP2, _, _, _, whitehouse, _),
    neighbour(HP1, HP2).

house(_, _, snake, winston, _, _).
house(_, _, _, kool, yellowhouse, _).
house(3, _, _, _, _, milk).
house(1, norwegian, _, _, _, _).

% chesterfield smoker is neighbour of fox owner

house(HP1, _, _, chesterfield, _, _) :-
    house(HP2, _, fox, _, _, _),
    neighbour(HP1, HP2).

% In the house next to the horse owner, they smoke kool

house(HP1, _, horse, _, _, _) :-
    house(HP2, _, _, kool, _, _),
    neighbour(HP1, HP2).

house(_, _, _, luckystrike, _, juice).

house(_, japanese, _, kent, _, _).

% norwegian lives next to bluehouse

house(HP1, norwegian, _, _, _, _) :-
    house(HP2, _, _, _, bluehouse, _),
    neighbour(HP1, HP2).

house(_, _, elephant, _, _, _).
house(_, _, _, _, _, vodka).

% ?- house(_, X, elephant, _, _, _).
% ?- house(_, X, _, _, _, vodka).
