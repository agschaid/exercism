-module(bob).

-export([response/1]).


response(String) -> 
    Trimmed = string:trim(String),
    case Trimmed of
        "" -> "Fine. Be that way!";
        _  -> case {asking(Trimmed), yelling(Trimmed)} of
                  {asking    , not_yelling} -> "Sure.";
                  {not_asking, yelling    } -> "Whoa, chill out!";
                  {asking    , yelling    } -> "Calm down, I know what I'm doing!";
                  _                         -> "Whatever."
              end
    end.

asking(String) -> 
    case lists:last(String) of 
        $? -> asking;
        _  -> not_asking
    end.

yelling(String) ->
    case {string:lowercase(String), string:uppercase(String)} of
        % No uppercase character present.
        % Maybe no alphabetical character at all.
        {String, _     } -> not_yelling;
        {_     , String} -> yelling;
         _               -> not_yelling
    end.

