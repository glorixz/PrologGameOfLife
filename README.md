# cs312prj2-game-of-life

Load the file using
[main].

Start the game by calling play(<mode>). To use the default mode, you can simply call 'play.'
List of modes:
    - default

If encountering error after starting swipl on windows,
"ERROR: source_sink `library(pce)' does not exist"
run the following in the interactive terminal:
assertz(file_search_path(library,pce('prolog/lib'))).
assertz(file_search_path(pce,swi(xpce))).

Alternatively, use 'swipl-win' instead of 'swipl'

Run `set_prolog_flag(answer_write_options,[max_depth(0)]).` to remove limit from swipl
