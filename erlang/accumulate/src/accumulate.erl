-module(accumulate).

-export([accumulate/2, test_version/0]).

accumulate(_Fn, []) -> [];
accumulate(Fn, [X| Xs]) -> [ Fn(X) | accumulate(Fn, Xs) ].

test_version() -> 1.
