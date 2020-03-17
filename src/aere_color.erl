-module(aere_color).
-export([ emph/2, emph/1
        , default/2, default/1
        , black/2, black/1
        , red/2, red/1
        , green/2, green/1
        , yellow/2, yellow/1
        , blue/2, blue/1
        , magenta/2, magenta/1
        , cyan/2, cyan/1
        , white/2, white/1
        , render_colored/2
        , reset/2, reset/1
        ]).
-include("aere_repl.hrl").


color_str(none, _) ->
    "";
color_str(_, reset) ->
    "\e[0m";
color_str(default, C) ->
    case C of
        default ->
            "\e[0;1m";
        emph ->
            "\e[1m";
        black ->
            "\e[1;30m";
        red ->
            "\e[1;31m";
        green ->
            "\e[1;32m";
        yellow ->
            "\e[1;33m";
        blue ->
            "\e[1;34m";
        magenta ->
            "\e[1;35m";
        cyan ->
            "\e[1;36m";
        white ->
            "\e[1;37m"
    end;
color_str(no_emph, C) ->
    case C of
        default ->
            "\e[0m";
        emph ->
            "\e[1m";
        black ->
            "\e[0;30m";
        red ->
            "\e[0;31m";
        green ->
            "\e[0;32m";
        yellow ->
            "\e[0;33m";
        blue ->
            "\e[0;34m";
        magenta ->
            "\e[0;35m";
        cyan ->
            "\e[0;36m";
        white ->
            "\e[0;37m"
    end.


with_color(red, Priority, Text) ->
    case get(wololo) of
        undefined ->
            {colored, red, Priority, Text};
        _ -> %% u didnt see it shhh
            {colored, blue, Priority, Text}
    end;
with_color(Color, Priority, Text) ->
    {colored, Color, Priority, Text}.


reset(Text) ->
    with_color(reset, 0, Text).

reset(Text, Priority) ->
    with_color(reset, Priority, Text).

default(Text) ->
    with_color(default, 0, Text).

default(Text, Priority) ->
    with_color(default, Priority, Text).

emph(Text) ->
    with_color(emph, 0, Text).

emph(Text, Priority) ->
    with_color(emph, Priority, Text).

black(Text) ->
    with_color(black, 0, Text).

black(Text, Priority) ->
    with_color(black, Priority, Text).

red(Text) ->
    with_color(red, 0, Text).

red(Text, Priority) ->
    with_color(red, Priority, Text).

green(Text) ->
    with_color(green, 0, Text).

green(Text, Priority) ->
    with_color(green, Priority, Text).

yellow(Text) ->
    with_color(yellow, 0, Text).

yellow(Text, Priority) ->
    with_color(yellow, Priority, Text).

blue(Text) ->
    with_color(blue, 0, Text).

blue(Text, Priority) ->
    with_color(blue, Priority, Text).

magenta(Text) ->
    with_color(magenta, 0, Text).

magenta(Text, Priority) ->
    with_color(magenta, Priority, Text).

cyan(Text) ->
    with_color(cyan, 0, Text).

cyan(Text, Priority) ->
    with_color(cyan, Priority, Text).

white(Text) ->
    with_color(white, 0, Text).

white(Text, Priority) ->
    with_color(white, Priority, Text).


render_colored(#repl_state{options = Opts}, C) ->
    render_colored(Opts, C);
render_colored(#options{colors = Coloring}, C) ->
    render_colored(Coloring, C);
render_colored(Coloring, C) ->
    lists:flatten(color_str(Coloring, default)
                  ++ render_colored(Coloring, C, default, 0)
                  ++ color_str(Coloring, reset)
                 ).
render_colored(_Coloring, [], _Prev, _Level) ->
    [];
render_colored(Coloring, [S|Rest], Prev, Level) when not is_integer(S) ->
    render_colored(Coloring, S, Prev, Level) ++ render_colored(Coloring, Rest, Prev, Level);
render_colored(Coloring, {colored, _Color, LevelNew, Text}, Prev, Level) when LevelNew < Level ->
    render_colored(Coloring, Text, Prev, Level);
render_colored(Coloring, {colored, Color, LevelNew, Text}, Prev, _Level) ->
    color_str(Coloring, Color) ++ render_colored(Coloring, Text, Color, LevelNew) ++ color_str(Coloring, Prev);
render_colored(_Coloring, S, _Prev, _Level) when is_list(S) ->
    S.


