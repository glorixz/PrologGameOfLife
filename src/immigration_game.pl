% Game of life rules:
% each cell interacts with its 8 possible neighbours.
% when a cell comes back to life, it takes on the color of its most dominant neighbor.
% dead cell = 0, live black cell = 1, live orange cell = 2

% cell_state_im(curr_state, neighbours, neighbour_majority_colour, next_state)
% a dead cell with 3 neighbours becomes live.
cell_state_im(0, 3, NC, NC).
% a dead cell with not 3 neighbours stays dead. 
cell_state_im(0, X, _, 0) :- X \= 3.
% a live cell with 2 neighbours stays live.
cell_state_im(C, 2, _, C) :- member(C, [1,2]).
% a live cell with 3 neighbours stays live.
cell_state_im(C, 3, _, C) :- member(C, [1,2]).
% a live cell with more than 3 or less than 2 neighbours dies.
cell_state_im(C, X, _, 0) :- member(C, [1,2]), X < 2 ; X > 3.


% ------------ Game State ------------ %
:- discontiguous update_cell/5.
update_cell(World, Canvas, X, Y, NewWorld) :-
    mode(immigration),
    cell_neighbours(X, Y, Neighbours),
    alive_neighbours(World, Neighbours, AliveNeighbours),
    length(AliveNeighbours, AliveCount),
    get_max_neighbours_colour(World, AliveNeighbours, Colour),
    nth0(Y, World, Row),
    nth0(X, Row, CellState),
    cell_state_im(CellState, AliveCount, Colour, NewCellState),
    set_cell(Canvas, X, Y, NewCellState, NewWorld).

alive_neighbours(World, Neighbours, AliveNeighbours) :-
    include(cell_is_alive(World), Neighbours, AliveNeighbours).

get_max_neighbours_colour(World, AliveNeighbours, Colour) :-
    include(cell_is_colour(World, 1), AliveNeighbours, Colour1Neighbours),
    include(cell_is_colour(World, 2), AliveNeighbours, Colour2Neighbours),
    larger_list((Colour1Neighbours,1), (Colour2Neighbours,2), Colour).

larger_list((List1,E1), (List2,E2), Element) :-
    length(List1, Length1),
    length(List2, Length2),
    (Length1 >= Length2 ->
        Element = E1 ;
        Element = E2).

:- discontiguous cell_is_alive/2.
cell_is_alive(World, (X, Y)) :-
    mode(immigration),
    nth0(Y, World, Row),
    nth0(X, Row, Value),
    (var(Value) -> fail ; (Value =:= 1 ; Value =:= 2)).

cell_is_colour(World, Colour, (X, Y)) :-
    nth0(Y, World, Row),
    nth0(X, Row, Value),
    (var(Value) -> fail ; Value =:= Colour).

% test_world([
%     [0,0,0,0,0],
%     [0,0,0,1,0],
%     [0,1,0,1,0],
%     [0,1,0,1,0],
%     [0,0,1,0,0]
% ]).

% test_world_coloured([
%     [0,0,0,0,0],
%     [0,0,0,2,0],
%     [0,1,0,2,0],
%     [0,1,0,1,0],
%     [0,0,1,0,0]
% ]).

% test_world_empty([
%     [0,0,0,0,0],
%     [0,0,0,0,0],
%     [0,0,0,0,0],
%     [0,0,0,0,0],
%     [0,0,0,0,0]
% ]).