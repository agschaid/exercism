-module(collatz_conjecture).

-export([steps/1]).


steps(N) when N =< 0 -> {error,"Only positive numbers are allowed" };
steps(N)             -> steps(0, N).

steps(Step, 1) -> Step;
steps(Step, N) ->
    Next_N = case N rem 2 of
                 0 -> N div 2;
                 1 -> (3*N) + 1
             end,
    steps(Step+1, Next_N).

