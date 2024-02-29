:- use_module(library(pce)).
:- include('ui/gui.pl').
:- include('src/game.pl').
:- include('src/immigration_game.pl').
:- include('src/worlds.pl').
:- dynamic mode/1, gamestate/1.

play(default) :- retractall(mode(_)), assertz(mode(default)), init.
play(immigration) :- retractall(mode(_)), assertz(mode(immigration)), init.
play :- play(default).

init :- 
    gameFrame(Frame),
    initGameWindow(Window, Frame),
    initGameGrid(Window),
    initButtons(Window),

    send(Window, open). % display the game board

