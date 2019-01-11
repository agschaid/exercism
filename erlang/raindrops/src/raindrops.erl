-module(raindrops).

-export([convert/1]).

-define(RAINDROP_RULES, [{3, "Pling"}, {5, "Plang"}, {7, "Plong"}]).

convert(Number) -> 
    Rule_Applies = fun({I, _T}) -> Number rem I == 0 end,

    case lists:filter(Rule_Applies, ?RAINDROP_RULES) of
        []    -> integer_to_list(Number);
        Rules -> lists:flatmap(fun({_I, T}) -> T end, Rules)
    end.
