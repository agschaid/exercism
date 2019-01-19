-module(bob).

-export([response/1]).


response(String) -> response_reversed(lists:reverse(clean(String))).

response_reversed("") -> "Fine. Be that way!";
response_reversed([$? | T]) -> 
	case is_yelling(T) of
		true  -> "Calm down, I know what I'm doing!";
		false -> "Sure."
	end;
response_reversed(T) -> 
	case is_yelling(T) of
	        true  -> "Whoa, chill out!";
		false -> "Whatever."
	end.

is_yelling(T) ->
	contains(T, "([A-Z])") and (not contains(T, "([a-z])")).

contains(String, Pattern) ->
	case re:replace(String, Pattern, [global, {return, list}]) of
		String -> false;
		_S     -> true
	end.

clean(String) ->
	re:replace(String, "(\\s+)", "", [global,{return,list}]).
