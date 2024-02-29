:- use_module(library(pce)).
:- dynamic squares/1.

% Pixel dimensions for the game board
windowDims(X, Y) :- X is 800, Y is 640.
% Grid dimensions, in units of squares.
gridDims(Col, Row) :- Col is 70, Row is 50.
% Size of a square, in pixels
squareSize(10).
% Center the grid horizontally
leftPad(LPad) :- LPad is 50.


% ========= Game loop rendering =========

% Wipe the window and redraw it according to the current game state.
redrawGame() :-
    gamestate(Gamestate),
    squares(Squares),
    redrawRows(Gamestate, Squares).

redrawRows([], _).
redrawRows([StateRow|T1], [SquareRow|T2]) :-
    redrawRow(StateRow, SquareRow),
    redrawRows(T1, T2).

redrawRow([], _).
redrawRow([Val|T1], [Square|T2]) :-
    colourSquare(Square,Val),
    redrawRow(T1, T2).


% Fill a square according to its value.
colourSquare(Square, 0) :-
    send(Square, fill_pattern, colour(white)).
colourSquare(Square, 1) :-
    send(Square, fill_pattern, colour(black)).
colourSquare(Square, 2) :-
    send(Square, fill_pattern, colour(orange)).

nextColour(0,1).
nextColour(1,0) :- mode(default).
nextColour(1,2) :- mode(immigration).
nextColour(2,0).

% ========= Initializing functions =========

% The game window frame; the parent of the Window.
gameFrame(Frame) :- new(Frame, frame('The Game of Life')).

% Creates the game window.
initGameWindow(Window, Frame) :-
    new(Window, picture),
    send(Frame, append, Window),
    windowDims(WX, WY),
    send(Window, size, size(WX, WY)),
    send(Window, scrollbars, none).

% Create Buttons
initButtons(Window) :-
    leftPad(LPad),

    new(StepSlider, slider('# Steps', 1, 10, 1)),
    send(StepSlider, size, size(170, 20)),
    send(StepSlider, position, point(LPad,520)),
    send(Window, display, StepSlider),

    new(StepBtn, button('Step')),
    send(StepBtn, size, size(50, 20)),
    send(StepBtn, position, point(LPad + 180, 520)),
    send(StepBtn, recogniser, click_gesture(left, '', single, message(@prolog, step, StepSlider?selection))),
    send(Window, display, StepBtn),

    new(ResetBtn, button('Reset')),
    send(ResetBtn, size, size(50, 20)),
    send(ResetBtn, position, point(LPad + 270, 520)),
    send(ResetBtn, recogniser, click_gesture(left, '', single, message(@prolog, clearGrid))),
    send(Window, display, ResetBtn),
    
    new(Menu, menu('Default maps')),
    send(Menu, position, point(LPad + 360, 520)),
    send(Menu, layout, vertical),
    send(Menu, append, 'Empty'),
    initModeSpecificOptions(Menu),
    send(Menu, message, message(@prolog, handleMapSelection, Menu?selection)),
    send(Window, display, Menu).

initModeSpecificOptions(Menu) :-
    mode(default),
    send(Menu, append, 'Random'),
    send(Menu, append, 'Glider'),
    send(Menu, append, 'Pulsar').

initModeSpecificOptions(Menu) :-
    mode(immigration),
    send(Menu, append, 'Random (Immigration)').

% Create and draw the game grid.
initGameGrid(Window) :-
    generateEmptyWorld(Gamestate),
    updateGamestate(Gamestate),
    drawGrid(Gamestate, Window).

% Create the graphical grid elements according to the Gamestate and add them to the Window.
drawGrid(Gamestate, Window) :- 
    drawGridRows(Gamestate, Window, 0, GridSquares),
    retractall(squares(_)),
    asserta(squares(GridSquares)).

drawGridRows([], _, _, _).
drawGridRows([CurrRow|T], Window, RowInd, [RowSquares|Rest]) :-
    drawRow(CurrRow, Window, 0, RowInd, RowSquares),
    NextRow is RowInd + 1,
    drawGridRows(T, Window, NextRow, Rest).

% Helper
drawRow([], _, _, _, _).
drawRow([SquareVal|T], Window, X, Y, [Square|Rest]) :-
    squareSize(L),
    leftPad(LPad),
    XPos is X * L + LPad,
    YPos is Y * L + L,
    
    new(Square, box(L, L)),
    send(Square, position, point(XPos, YPos)),
    send(Square, colour(gray)),         % line colour
    colourSquare(Square, SquareVal),    % fill colour
    send(Square, recogniser, click_gesture(left, '', single, message(@prolog, updateSquare, Square, X, Y))),

    send(Window, display, Square),
    NextX is X + 1,
    drawRow(T, Window, NextX, Y, Rest).


% ========== Events ==========

% On click, change the fill value of a square.
updateSquare(Square, Col, Row) :-
    gamestate(Gamestate),
    nth0(Row, Gamestate, RowList),
    nth0(Col, RowList, Val),
    nextColour(Val, NewVal),

    updateCell(Gamestate, Row, Col, NewVal, NewGamestate),
    updateGamestate(NewGamestate),
    
    colourSquare(Square, NewVal).

% Wipe all squares.
clearGrid() :-
    generateEmptyWorld(Gamestate),
    updateGamestate(Gamestate),
    redrawGame().

handleMapSelection('Empty') :-
    clearGrid().

handleMapSelection('Random') :-
    generateEmptyWorld(EmptyWorld),
    random_world(default, 1000, EmptyWorld, Gamestate),
    updateGamestate(Gamestate),
    redrawGame().

handleMapSelection('Random (Immigration)') :-
    generateEmptyWorld(EmptyWorld),
    random_world(colour, 1000, EmptyWorld, Gamestate),
    updateGamestate(Gamestate),
    redrawGame().

handleMapSelection('Glider') :-
    generateEmptyWorld(EmptyWorld),
    glider_world(EmptyWorld, Gamestate),
    updateGamestate(Gamestate),
    redrawGame().

handleMapSelection('Pulsar') :-
    generateEmptyWorld(EmptyWorld),
    pulsar_world(EmptyWorld, Gamestate),
    updateGamestate(Gamestate),
    redrawGame().