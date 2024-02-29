random_world(_, 0, World, World).
random_world(default, N, World, RandomWorld) :-
    random_world_helper(1, World, PartialWorld),
    N1 is N - 1,
    random_world(default, N1, PartialWorld, RandomWorld).

random_world(colour, N, World, RandomWorld) :-
    random(1,3,RandomColour),
    random_world_helper(RandomColour, World, PartialWorld),
    N1 is N - 1,
    random_world(colour, N1, PartialWorld, RandomWorld).

random_world_helper(Colour, World, NewWorld) :-
    gridDims(Col, Row),
    UpperX is Col - 1,
    UpperY is Row - 1,
    random_between(1, UpperX, X),
    random_between(1, UpperY, Y),
    set_cell(World, X, Y, Colour, NewWorld).

pulsar_world(World, PulsarWorld) :-
    set_cell(World, 35, 27, 1, NewWorld1),
    set_cell(NewWorld1, 35, 28, 1, NewWorld2), 
    set_cell(NewWorld2, 35, 29, 1, NewWorld3), 
    set_cell(NewWorld3, 35, 33, 1, NewWorld4), 
    set_cell(NewWorld4, 35, 34, 1, NewWorld5), 
    set_cell(NewWorld5, 35, 35, 1, NewWorld6),
    set_cell(NewWorld6, 37, 25, 1, NewWorld7), 
    set_cell(NewWorld7, 37, 30, 1, NewWorld8), 
    set_cell(NewWorld8, 37, 32, 1, NewWorld9), 
    set_cell(NewWorld9, 37, 37, 1, NewWorld10),
    set_cell(NewWorld10, 38, 25, 1, NewWorld11), 
    set_cell(NewWorld11, 38, 30, 1, NewWorld12), 
    set_cell(NewWorld12, 38, 32, 1, NewWorld13), 
    set_cell(NewWorld13, 38, 37, 1, NewWorld14),
    set_cell(NewWorld14, 39, 25, 1, NewWorld15), 
    set_cell(NewWorld15, 39, 30, 1, NewWorld16), 
    set_cell(NewWorld16, 39, 32, 1, NewWorld17), 
    set_cell(NewWorld17, 39, 37, 1, NewWorld18),
    set_cell(NewWorld18, 40, 27, 1, NewWorld19), 
    set_cell(NewWorld19, 40, 28, 1, NewWorld20), 
    set_cell(NewWorld20, 40, 29, 1, NewWorld21), 
    set_cell(NewWorld21, 40, 33, 1, NewWorld22), 
    set_cell(NewWorld22, 40, 34, 1, NewWorld23), 
    set_cell(NewWorld23, 40, 35, 1, NewWorld24),
    set_cell(NewWorld24, 42, 27, 1, NewWorld25), 
    set_cell(NewWorld25, 42, 28, 1, NewWorld26), 
    set_cell(NewWorld26, 42, 29, 1, NewWorld27), 
    set_cell(NewWorld27, 42, 33, 1, NewWorld28), 
    set_cell(NewWorld28, 42, 34, 1, NewWorld29), 
    set_cell(NewWorld29, 42, 35, 1, NewWorld30),
    set_cell(NewWorld30, 43, 25, 1, NewWorld31), 
    set_cell(NewWorld31, 43, 30, 1, NewWorld32), 
    set_cell(NewWorld32, 43, 32, 1, NewWorld33), 
    set_cell(NewWorld33, 43, 37, 1, NewWorld34),
    set_cell(NewWorld34, 44, 25, 1, NewWorld35), 
    set_cell(NewWorld35, 44, 30, 1, NewWorld36), 
    set_cell(NewWorld36, 44, 32, 1, NewWorld37), 
    set_cell(NewWorld37, 44, 37, 1, NewWorld38),
    set_cell(NewWorld38, 45, 25, 1, NewWorld39), 
    set_cell(NewWorld39, 45, 30, 1, NewWorld40), 
    set_cell(NewWorld40, 45, 32, 1, NewWorld41), 
    set_cell(NewWorld41, 45, 37, 1, NewWorld42),
    set_cell(NewWorld42, 47, 27, 1, NewWorld43), 
    set_cell(NewWorld43, 47, 28, 1, NewWorld44), 
    set_cell(NewWorld44, 47, 29, 1, NewWorld45), 
    set_cell(NewWorld45, 47, 33, 1, NewWorld46), 
    set_cell(NewWorld46, 47, 34, 1, NewWorld47), 
    set_cell(NewWorld47, 47, 35, 1, PulsarWorld).

glider_world(World, GliderWorld) :-
    set_cell(World, 34, 24, 1, NewWorld1),
    set_cell(NewWorld1, 35, 25, 1, NewWorld2),
    set_cell(NewWorld2, 35, 26, 1, NewWorld3),
    set_cell(NewWorld3, 34, 26, 1, NewWorld4),
    set_cell(NewWorld4, 33, 26, 1, GliderWorld).