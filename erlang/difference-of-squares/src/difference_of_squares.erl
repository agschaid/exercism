-module(difference_of_squares).

-export([sum_of_squares/1, square_of_sum/1, difference_of_squares/1, test_version/0]).

sum_of(N, ElementModifier) ->
    lists:foldl(fun(X, Sum) -> X + Sum end,
                0,
                [ElementModifier(X) || X <- lists:seq(1, N)]
               ).

square(X) -> X*X.
id(X) -> X.

sum_of_squares(N) ->
  sum_of(N, fun square/1).

square_of_sum(N) ->
  square(sum_of(N, fun id/1)).

difference_of_squares(N) ->
  square_of_sum(N) - sum_of_squares(N).

test_version() -> 1.
