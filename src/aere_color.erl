-module(aere_color).
-export([ emph/1
        , default/1
        , black/1
        , red/1
        , green/1
        , yellow/1
        , blue/1
        , magenta/1
        , cyan/1
        , white/1
        , render_colored/1, render_colored/2
        , reset/1
        ]).

color_str(none, _) ->
    "";
color_str(_, reset) ->
    "\e[0m";
color_str(emph, C) ->
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


with_color(red, Text) ->
    case get(wololo) of
        undefined ->
            {colored, red, Text};
        _ -> %% u didnt see it shhh
            {colored, blue, Text}
    end;
with_color(Color, Text) ->
    {colored, Color, Text}.


reset(Text) ->
    with_color(reset, Text).

default(Text) ->
    with_color(default, Text).

emph(Text) ->
    with_color(emph, Text).

black(Text) ->
    with_color(black, Text).

red(Text) ->
    with_color(red, Text).

green(Text) ->
    with_color(green, Text).

yellow(Text) ->
    with_color(yellow, Text).

blue(Text) ->
    with_color(blue, Text).

magenta(Text) ->
    with_color(magenta, Text).

cyan(Text) ->
    with_color(cyan, Text).

white(Text) ->
    with_color(white, Text).


render_colored(C) ->
    render_colored(emph, C).
render_colored(Coloring, C) ->
    lists:flatten(color_str(Coloring, default)
                  ++ render_colored1(Coloring, C)
                  ++ color_str(Coloring, reset)
                 ).
render_colored1(_Coloring, []) ->
    [];
render_colored1(Coloring, [C|Rest]) when is_integer(C) ->
    [C|render_colored(Coloring, Rest)];
render_colored1(Coloring, [S|Rest]) ->
    render_colored(Coloring, S) ++ render_colored(Coloring, Rest);
render_colored1(Coloring, {colored, Color, Text}) ->
    color_str(Coloring, Color) ++ Text ++ color_str(Coloring, reset).
