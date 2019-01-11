-module(accumulate).

-export([accumulate/2, test_version/0]).

accumulate(Fn, Xs) -> rec_acc(Fn, [], lists:reverse(Xs)).

rec_acc(_Fn, Acc, []) -> Acc;
rec_acc(Fn, Acc, [X|Xs]) -> rec_acc(Fn, 
                                    [Fn(X)|Acc],
                                    Xs
                                   ).

test_version() -> 1.
