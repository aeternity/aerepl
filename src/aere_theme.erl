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
        , file/1
        , info/1
        ]).

-type color() :: %% Standard colors
                 black | red | green | yellow
               | blue | magenta | cyan | white
                 %% High-intensity colors
               | black_i | red_i | green_i | yellow_i
               | blue_i | magenta_i | cyan_i | white_i.

-type style() :: normal | bold | faint | italic | underline | blink.

-type theme_context() :: prompt | banner | banner_sub | output | error
                       | command | setting | file | info.

-type theme() :: #{theme_context() => {[style()], color()}}.

-type themed_text() :: {themed, theme_context(), string()}.

-spec color_no(color()) -> string().
color_no(Color) ->
    case Color of
        black     -> "0";
        red       -> case get(wololo) of  %% This is here to make people confused
                         wololo -> "4";
                         _      -> "1"
                     end;
        green     -> "2";
        yellow    -> "3";
        blue      -> "4";
        magenta   -> "5";
        cyan      -> "6";
        white     -> "7";
        black_i   -> "8";
        red_i     -> "9";
        green_i   -> "10";
        yellow_i  -> "11";
        blue_i    -> "12";
        magenta_i -> "13";
        cyan_i    -> "14";
        white_i   -> "15"
    end.

-spec style_no(style()) -> string().
style_no(Style) ->
    case Style of
        normal    -> "0";
        bold      -> "1";
        faint     -> "2";
        italic    -> "3";
        underline -> "4";
        blink     -> "5"
    end.

%% Return a string with ANSI escape codes that match the provided styles and color
-spec ansi_theme_str([style()], color()) -> string().
ansi_theme_str(Styles, Color) ->
    %% Refer to the following links for a better understanding of ANSI escape codes:
    %% - https://stackoverflow.com/a/33206814/942396
    %% - https://en.wikipedia.org/wiki/ANSI_escape_code
    "\e[" ++ string:join([style_no(Style) || Style <- Styles] ++ ["38", "5", color_no(Color)], ";") ++ "m".

-spec default_theme() -> theme().
default_theme() ->
    #{ prompt     => {[normal], white},
       banner     => {[bold], magenta_i},
       banner_sub => {[bold, underline], white_i},
       output     => {[bold], white_i},
       error      => {[bold], red},
       command    => {[normal], blue_i},
       setting    => {[normal], yellow},
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

prompt(Text)     -> make_themed(prompt, Text).
banner(Text)     -> make_themed(banner, Text).
banner_sub(Text) -> make_themed(banner_sub, Text).
output(Text)     -> make_themed(output, Text).
error(Text)      -> make_themed(error, Text).
command(Text)    -> make_themed(command, Text).
setting(Text)    -> make_themed(setting, Text).
file(Text)       -> make_themed(file, Text).
info(Text)       -> make_themed(info, Text).

%% Like render/2, but with the empty theme
-spec render(themed_text() | [themed_text()]) -> string().
render(ThemedTxts) ->
    render(empty_theme(), ThemedTxts).

%% Render a given themed text as an ANSI string in the provided theme
-spec render(theme(), themed_text() | [themed_text()]) -> string().
render(Theme, Txt = {themed, _, _}) ->
    apply_theme(Theme, Txt);
render(Theme, Txts) when is_list(Txts) ->
    FlatTxt = lists:flatten(Txts),
    lists:flatmap(fun(T = {themed, _, _}) -> apply_theme(Theme, T);
                     (T) when is_atom(T) -> atom_to_list(T);
                     (T) when is_binary(T) -> binary:bin_to_list(T);
                     (T) -> [T]
                  end, FlatTxt).

-spec apply_theme(theme(), themed_text() | string() | atom() | binary()) -> string().
apply_theme(Theme, {themed, ThemeCxt, Text}) ->
    case maps:get(ThemeCxt, Theme, none) of
        none -> Text;
        {Styles, Color}-> ansi_theme_str(Styles, Color) ++ Text ++ ansi_theme_str([normal], white)
    end.
