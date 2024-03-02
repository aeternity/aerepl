-module(aere_options).

-export([option_parse_rules/0, parse_option/2, format_option_scheme/1]).

option_parse_rules() ->
    [ {call_gas, {valid, integer, fun(I) -> I >= 0 end, "non-neg"}}
    , {call_value, {valid, integer, fun(I) -> I >= 0 end, "non-neg"}}
    , {print_gas, boolean}
    , {print_format, {atom, [sophia,json,fate]}}
    , {print_unit, boolean}
    , {print_type, boolean}
    , {loc_forwards, {valid, integer, fun(I) -> I >= 0 end, "non-neg"}}
    , {loc_backwards, {valid, integer, fun(I) -> I >= 0 end, "non-neg"}}
    ].

parse_option(Option, Args) ->
    case proplists:get_value(Option, option_parse_rules(), unknown) of
        unknown ->
            error;
        Scheme -> parse_option_args(Scheme, Args)
    end.

parse_option_args(Scheme, Args) ->
    case {Scheme, Args} of
        {{valid, Scheme1, Valid, _}, _} ->
            case parse_option_args(Scheme1, Args) of
                error -> error;
                X -> case Valid(X) of
                         true -> X;
                         false -> error
                     end
            end;
        {boolean, ["true"]} ->
            true;
        {boolean, ["false"]} ->
            false;
        {integer, [A]} ->
            try list_to_integer(string:trim(A))
            catch error:badarg -> error
            end;
        {{atom, Ats}, [A]} ->
            %% Valid atoms should have been created while defining rules
            try list_to_existing_atom(string:trim(A)) of
                At -> case lists:member(At, Ats) of
                          true -> At;
                          _ -> error
                      end
            catch error:badarg -> error
            end;
        _ ->
            error
    end.

format_option_scheme(integer) -> "INTEGER";
format_option_scheme(boolean) -> "BOOLEAN";
format_option_scheme({atom, Ats}) -> string:join(lists:map(fun atom_to_list/1, Ats), "|");
format_option_scheme({valid, Kind, _, Expl}) ->
    format_option_scheme(Kind) ++ "(" ++ Expl ++ ")".
