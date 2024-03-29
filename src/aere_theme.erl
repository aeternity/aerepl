-module(aere_theme).

-export([ render/1
        , render/2
        ]).

-export([ empty_theme/0
        , default_theme/0
        ]).

-export([ prompt/1
        , banner/1
        , banner_sub/1
        , output/1
        , error/1
        , command/1
        , setting/1
        , setting_arg/1
        , file/1
        , info/1
        ]).

-export_type([ renderable/0
             , theme/0
             ]).

-type color() :: %% Standard colors
                 black | red | green | yellow
               | blue | magenta | cyan | white
                 %% High-intensity colors
               | black_i | red_i | green_i | yellow_i
               | blue_i | magenta_i | cyan_i | white_i.

-type style() :: normal | bold | faint | italic | underline | blink.

-type theme_context() :: prompt | banner | banner_sub | output | error
                       | command | setting | setting_arg | file | info.

-type theme() :: #{theme_context() => {[style()], color()}}.

-type themed_text() :: {themed, theme_context(), string()}.

-type renderable() :: themed_text() | [renderable()].


-spec ansi_color_no(color()) -> string().
ansi_color_no(black)     -> "0";
ansi_color_no(red) ->
    case get(wololo) of  %% This is here to make people confused
        wololo -> ansi_color_no(blue);
        _                -> "1"
    end;
ansi_color_no(green)     -> "2";
ansi_color_no(yellow)    -> "3";
ansi_color_no(blue)      -> "4";
ansi_color_no(magenta)   -> "5";
ansi_color_no(cyan)      -> "6";
ansi_color_no(white)     -> "7";
ansi_color_no(black_i)   -> "8";
ansi_color_no(red_i)     -> "9";
ansi_color_no(green_i)   -> "10";
ansi_color_no(yellow_i)  -> "11";
ansi_color_no(blue_i)    -> "12";
ansi_color_no(magenta_i) -> "13";
ansi_color_no(cyan_i)    -> "14";
ansi_color_no(white_i)   -> "15".


-spec style_no(style()) -> string().
style_no(normal)    -> "0";
style_no(bold)      -> "1";
style_no(faint)     -> "2";
style_no(italic)    -> "3";
style_no(underline) -> "4";
style_no(blink)     -> "5".


%% Return a string with ANSI escape codes that match the provided styles and color
-spec ansi_theme_str([style()], color()) -> string().
ansi_theme_str(Styles, Color) ->
    %% Refer to the following links for a better understanding of ANSI escape codes:
    %% - https://stackoverflow.com/a/33206814/942396
    %% - https://en.wikipedia.org/wiki/ANSI_escape_code
    "\e[" ++ string:join([style_no(Style) || Style <- Styles] ++ ["38", "5", ansi_color_no(Color)], ";") ++ "m".


-spec default_theme() -> theme().
default_theme() ->
    #{ prompt     => {[normal], white},
       banner     => {[bold], magenta_i},
       banner_sub => {[bold, underline], white_i},
       output     => {[bold], white_i},
       error      => {[bold], red},
       command    => {[normal], blue_i},
       setting    => {[normal], yellow_i},
       setting_arg => {[italic], white},
       file       => {[normal], yellow_i},
       info       => {[italic, faint], white_i}
    }.


-spec empty_theme() -> theme().
empty_theme() ->
    #{}.


-spec make_themed(theme_context(), binary() | string()) -> themed_text().
make_themed(ThemeCxt, B) when is_binary(B) ->
    make_themed(ThemeCxt, binary:bin_to_list(B));
make_themed(ThemeCxt, Text) when is_list(Text) ->
    {themed, ThemeCxt, Text}.


-spec prompt(string()) -> themed_text().
prompt(Text) -> make_themed(prompt, Text).


-spec banner(string()) -> themed_text().
banner(Text) -> make_themed(banner, Text).


-spec banner_sub(string()) -> themed_text().
banner_sub(Text) -> make_themed(banner_sub, Text).


-spec output(string()) -> themed_text().
output(Text) -> make_themed(output, Text).


-spec error(string()) -> themed_text().
error(Text) -> make_themed(error, Text).


-spec command(string()) -> themed_text().
command(Text) -> make_themed(command, Text).


-spec setting(string()) -> themed_text().
setting(Text) -> make_themed(setting, Text).


-spec setting_arg(string()) -> themed_text().
setting_arg(Text) -> make_themed(setting_arg, Text).


-spec file(string()) -> themed_text().
file(Text) -> make_themed(file, Text).


-spec info(string()) -> themed_text().
info(Text) -> make_themed(info, Text).


%% Like render/2, but with the empty theme
-spec render(renderable()) -> binary().
render(ThemedTxts) ->
    render(empty_theme(), ThemedTxts).


%% Render a given renderable as an ANSI escaped byte string in the provided theme
-spec render(theme(), renderable()) -> binary().
render(Theme, ThemedTxts) ->
    Str = render_str(Theme, ThemedTxts),
    list_to_binary(Str).


-spec render_str(theme(), renderable()) -> string().
render_str(Theme, ThemedTxt = {themed, _, _}) ->
    render_str(Theme, [ThemedTxt]);
render_str(Theme, ThemedTxts) when is_list(ThemedTxts) ->
    AnsiStr = lists:flatten(lists:map(fun(T) -> apply_theme(Theme, T) end, lists:flatten(ThemedTxts))),
    string:trim(AnsiStr, trailing, unicode_util:whitespace()).


-spec apply_theme(theme(), themed_text()) -> string().
apply_theme(Theme, {themed, ThemeCxt, Text}) ->
    case maps:get(ThemeCxt, Theme, none) of
        none -> Text;
        {Styles, Color} -> ansi_theme_str(Styles, Color) ++ Text ++ ansi_theme_str([normal], white)
    end.
