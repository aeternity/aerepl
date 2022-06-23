-module(aere_color).

-export([ render_colored/1
        , render_colored/2
        ]).

-export([ coloring_none/0
        , coloring_default/0
        ]).

-export([ prompt/1
        , banner/1
        , banner_sub/1
        , output/1
        , error/1
        , command/1
        , setting/1
        , file/1
        , info/1
        ]).

color_no(Color) ->
    case Color of
        black    -> "0";
        red      -> "1";
        green    -> "2";
        yellow   -> "3";
        blue     -> "4";
        magenta   -> "5";
        cyan     -> "6";
        white    -> "7";
        black_i  -> "8";
        red_i    -> "9";
        green_i  -> "10";
        yellow_i -> "11";
        blue_i   -> "12";
        magenta_i -> "13";
        cyan_i   -> "14";
        white_i  -> "15"
    end.

style_no(Style) ->
    case Style of
        normal    -> "0";
        bold      -> "1";
        faint     -> "2";
        italic    -> "3";
        underline -> "4";
        blink     -> "5"
    end.

color_str(Color) ->
    color_str([], Color).

color_str(Styles, Color) ->
    "\e[" ++ string:join([style_no(Style) || Style <- Styles] ++ ["38", "5", color_no(Color)], ";") ++ "m".

coloring_default() ->
    #{ prompt => {[normal], white},
       banner => {[bold], magenta_i},
       banner_sub => {[bold, underline], white_i},
       output => {[bold], white_i},
       error  => {[bold], red},
       command => blue_i,
       setting => yellow,
       file => yellow_i,
       info => {[italic, faint], white}
    }.

coloring_none() ->
    #{}.

s(_, "") -> "";
s(Color, Text) when is_list(Text) ->
    [{colored, Color, Text}].

prompt(Text) -> s(prompt, Text).
banner(Text) -> s(banner, Text).
banner_sub(Text) -> s(banner_sub, Text).
output(Text) -> s(output, Text).
error(Text) -> s(error, Text).
command(Text) -> s(command, Text).
setting(Text) -> s(setting, Text).
file(Text) -> s(file, Text).
info(Text) -> s(info, Text).

render_colored(C) ->
    render_colored(normal, C).
render_colored(Coloring, C) ->
    lists:flatten(apply_color(Coloring, C)).

apply_color(_Coloring, []) ->
    [];
apply_color(Coloring, [C|Rest]) when is_integer(C) ->
    [C|apply_color(Coloring, Rest)];
apply_color(Coloring, [S|Rest]) ->
    apply_color(Coloring, S) ++ apply_color(Coloring, Rest);
apply_color(Coloring, {colored, Color, Text}) when is_list(Text) ->
    case maps:get(Color, Coloring, none) of
        none -> Text;
        {Style, ColorStr} -> color_str(Style, ColorStr) ++ Text ++ color_str([normal], white);
        ColorStr -> color_str(ColorStr) ++ Text ++ color_str([normal], white)
    end.
