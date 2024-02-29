# Conway's Game of Life
This is a Prolog implementation of Conway's Game of Life.

## How to run
Start up a session using ```swipl``` or ```swipl-win``` (recommended).

Load the file using
```[main].```

Start the game by calling ```play(<mode>).``` To use the default mode, you can simply call ```play.```

List of modes:
- default
- immigration

If encountering error after starting swipl on windows,
"ERROR: source_sink `library(pce)' does not exist"
run the following in the interactive terminal:
```
assertz(file_search_path(library,pce('prolog/lib'))).
assertz(file_search_path(pce,swi(xpce))).
```

For best results, use ```swipl-win``` instead of ```swipl```

For debugging purposes, run ```set_prolog_flag(answer_write_options,[max_depth(0)]).``` to remove limit on console output from swipl.

## How to play
You can toggle the state of any square in the gameboard by clicking on the square you want to change. Speed up the process by choosing a starting map, such as Pulsar.

Once you're satisfied, set the game in motion by pressing the Step button. You can increase the number of steps that happen in succession by moving the slider.


## Default mode
![Alt](/img/pulsar-demo.jpg "Default mode")

## Immigration mode
This is a variation of the game that uses multiple colours.
![Alt](/img/immigration-demo.jpg "Immigration mode")
