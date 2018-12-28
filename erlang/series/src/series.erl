-module(series).

-export([from_string/2, test_version/0]).

from_string(Width, String) ->
    Length = string:len(String),

    F = fun F(Index) ->
              if 
                  Width + Index > Length -> [];
                  true -> [string:slice(String, Index, Width) | F(Index+1)]
              end
          end,

    F(0).

test_version() -> 1.
