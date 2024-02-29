% Game of life rules:
% each cell interacts with its 8 possible neighbours
% dead cell = 0, live cell = 1

% cell_state(alive?, neighbours, alive?)
cell_state(0, 3, 1). % a dead cell with 3 neighbours becomes live.
cell_state(1, 2, 1). % a live cell with 2 neighbours stays live.
cell_state(1, 3, 1). % a live cell with 3 neighbours stays live.
cell_state(1, X, 0) :- X < 2 ; X > 3. % a live cell with more than 3 or less than 2 neighbours dies.
cell_state(0, X, 0) :- X \= 3. % a dead cell with not 3 neighbours stays dead. 

% Neighbours of a cell at X and Y
cell_neighbours(X,Y,N) :-
    top(X,Y,T),
    bottom(X,Y,B),
    left(X,Y,L),
    right(X,Y,R),
    top_right(X,Y,TR),
    top_left(X,Y,TL),
    bottom_right(X,Y,BR),
    bottom_left(X,Y,BL),
    list_to_set([T, B, L, R, TR, TL, BR, BL], N).

top(X,Y, (X,T)) :- T is Y + 1.
bottom(X,Y, (X,B)) :- B is Y - 1.
left(X,Y, (L,Y)) :- L is X - 1.
right(X,Y, (R,Y)) :- R is X + 1.
top_right(X,Y, (R,T)) :- R is X + 1, T is Y + 1.
top_left(X,Y, (L,T)) :- L is X - 1, T is Y + 1.
bottom_right(X,Y, (R,B)) :- R is X + 1, B is Y - 1.
bottom_left(X,Y, (L,B)) :- L is X - 1, B is Y - 1.

% ------------ Game Loop ------------ %
step(1) :-
    gamestate(World),
	update_world(World),
	redrawGame().
step(NumSteps) :-
	gamestate(World),
	update_world(World),
	redrawGame(),
   	(NumSteps > 1 ->
		send(timer(0.5), delay),
		StepsLeft is NumSteps - 1,
		step(StepsLeft),
		free(timer)
   	).

% ------------ Game State ------------ %

% Update the predicate representing the gamestate.
updateGamestate(Gamestate) :-
    retractall(gamestate(_)),
    asserta(gamestate(Gamestate)).

update_world(World) :-
    generateEmptyWorld(EmptyWorld),
    update_world_helper(World, EmptyWorld, NewWorld, 0, 0),
    updateGamestate(NewWorld).

update_world_helper(World, Canvas, NewWorld, X, Y) :-
    length(World, Rows),
    nth0(Y, World, Row),
    length(Row, Cols),
    update_cell(World, Canvas, X, Y, UpdatedCanvas),
    % if we are at the last cell, then return UpdatedCanvas
    (X =:= Cols-1, Y =:= Rows-1 ->
        NewWorld = UpdatedCanvas
    ;
        % if we are at the last cell in row, reset to 0 and move to next row
        (X =:= Cols-1 ->
            NewX is 0,
            NewY is Y+1
        ;
        % else move to next cell
            NewX is X+1,
            NewY is Y
        ),
    % else update world
        update_world_helper(World, UpdatedCanvas, NewWorld, NewX, NewY)
    ).

update_cell(World, Canvas, X, Y, NewWorld) :-
    mode(default),
    cell_neighbours(X, Y, Neighbours),
    count_alive_neighbours(World, Neighbours, AliveCount),
    nth0(Y, World, Row),
    nth0(X, Row, CellState),
    cell_state(CellState, AliveCount, NewCellState),
    set_cell(Canvas, X, Y, NewCellState, NewWorld).

count_alive_neighbours(World, Neighbours, AliveCount) :-
    include(cell_is_alive(World), Neighbours, AliveNeighbours),
    length(AliveNeighbours, AliveCount).

cell_is_alive(World, (X, Y)) :-
    mode(default),
    nth0(Y, World, Row),
    nth0(X, Row, 1).

set_cell(World, X, Y, Value, NewWorld) :-
    nth0(Y, World, Row),
    replace_nth(X, Row, Value, NewRow),
    replace_nth(Y, World, NewRow, NewWorld).

replace_nth(0, [_|T], Value, [Value|T]).
replace_nth(Index, [H|T], Value, [H|NewT]) :-
    Index > 0,
    NextIndex is Index - 1,
    replace_nth(NextIndex, T, Value, NewT).


% Update the cell at the specified row and column.
updateCell(Gamestate, Row, Col, NewVal, NewGamestate) :-
    replaceNth(Row, OldRow, NewRow, Gamestate, NewGamestate),
    replaceNth(Col, _OldVal, NewVal, OldRow, NewRow).

% General-purpose replaceNth function that can be chained
replaceNth(N, OldElem, NewElem, List, NewList) :-
    length(L1, N),
    append(L1, [OldElem|Rest], List),
    append(L1, [NewElem|Rest], NewList).

% test_world([
%     [0,0,0,0,0],
%     [0,0,0,1,0],
%     [0,1,0,1,0],
%     [0,1,0,1,0],
%     [0,0,1,0,0]
% ]).


% ========== Gamestate generation ==========

% Initialize a Gamestate as a 2D array of unfilled squares, represented as zeroes.
generateEmptyWorld(Gamestate) :-
    gridDims(Col, Row),
    length(Gamestate, Row),
    createRows(Col, Gamestate).

% Helper; create rows with length given by Col.
createRows(_, []).
createRows(Col, [Row|T]) :-
    length(Row, Col),
    maplist(initSquare, Row),
    createRows(Col, T).

% The initial value of a square can be modified here.
initSquare(0).