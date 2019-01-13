-module(raindrops).

-export([convert/1]).

-define(RAINDROP_RULES, [{3, "Pling"}, {5, "Plang"}, {7, "Plong"}]).

convert(Number) -> 
    Rule_Applies = fun({Divisor, _T}) -> Number rem Divisor == 0 end,

    case [R || R <- ?RAINDROP_RULES, Rule_Applies(R)] of
        []    -> integer_to_list(Number);
        Rules -> [Char || {_D, Text} <- Rules, Char <- Text]
    end.
