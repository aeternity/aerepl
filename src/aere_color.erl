-module(aere_color).
-export([ reset/0
        , emph/0, emph/1
        , black/0, black/1
        , red/0, red/1
        , green/0, green/1
        , yellow/0, yellow/1
        , blue/0, blue/1
        , magenta/0, magenta/1
        , cyan/0, cyan/1
        , white/0, white/1
        ]).

reset() ->
    "\e[0m".

emph() ->
    "\e[1m".

black() ->
    "\e[1;30m".

red() ->
    "\e[1;31m".

green() ->
    "\e[1;32m".

yellow() ->
    "\e[1;33m".

blue() ->
    "\e[1;34m".

magenta() ->
    "\e[1;35m".

cyan() ->
    "\e[1;36m".

white() ->
    "\e[1;37m".

with_color(Color, Text) ->
    Color ++ Text ++ reset().

emph(Text) ->
    with_color(emph(), Text).

black(Text) ->
    with_color(black(), Text).

red(Text) ->
    with_color(red(), Text).

green(Text) ->
    with_color(green(), Text).

yellow(Text) ->
    with_color(yellow(), Text).

blue(Text) ->
    with_color(blue(), Text).

magenta(Text) ->
    with_color(magenta(), Text).

cyan(Text) ->
    with_color(cyan(), Text).

white(Text) ->
    with_color(white(), Text).
