-module(aere_options).

-export([option_parse_rules/0, parse_option/2]).

option_parse_rules() ->
    [ {display_gas, boolean}
    , {call_gas, {valid, integer, fun(I) -> I >= 0 end, "non-neg"}}
    , {call_value, {valid, integer, fun(I) -> I >= 0 end, "non-neg"}}
    , {print_format, {atom, [sophia,json,fate]}}
    , {print_unit, boolean}
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
