-module(custom_set).

-export([add/2, contains/2, difference/2, disjoint/2, empty/1, equal/2, from_list/1, intersection/2, subset/2,
	 union/2]).


add(Elem, empty) -> {Elem, empty, empty};
add(Elem, {Elem, L, R}) -> {Elem, L, R};
add(Elem, {E, L, R}) when Elem < E -> {E, add(Elem, L), R};
add(Elem, {E, L, R}) when Elem > E -> {E, L, add(Elem, R)}.

contains(_Elem, empty) -> false;
contains(Elem, {Elem, _L, _R}) -> true;
contains(Elem, {E, L, _R}) when Elem < E -> contains(Elem, L);
contains(Elem, {E, _L, R}) when Elem > E -> contains(Elem, R).



empty(empty) -> true;
empty(_Set)  -> false.

equal(Set1, Set2) -> subset(Set1, Set2) andalso subset(Set2, Set1).

from_list(List) -> lists:foldl(fun add/2, empty, List).

fold(_Fun, Acc, empty) -> Acc;
fold(Fun, Acc, {E, L, R}) ->
    Acc_L = fold(Fun, Acc, L),
    Acc_E = Fun(E, Acc_L),
    fold(Fun, Acc_E, R).

filter(Predicate, Set) -> fold(
                            fun(E, Acc) ->
                                case Predicate(E) of
                                    false -> Acc;
                                    true  -> add(E, Acc)
                                end
                            end,
                            empty,
                            Set
                           ).

intersection(Set1, Set2) -> filter( fun(E) -> contains(E, Set2) end, Set1 ).

difference(Set1, Set2) -> filter( fun(E) -> not contains(E, Set2) end, Set1 ).

for_all(empty, _Predicate) -> true;
for_all({E, L, R}, Predicate) ->
    case Predicate(E) of
        false -> false;
        true  -> for_all(L, Predicate) andalso for_all(R, Predicate)
    end.


subset(Set1, Set2) -> for_all(Set1, fun(E) -> contains(E, Set2) end).

disjoint(Set1, Set2) -> for_all(Set1, fun(E) -> not contains(E, Set2) end).

union(empty, Set2) -> Set2;
union(Set1, empty) -> Set1;
union(Set1, Set2) -> fold( fun add/2, Set1, Set2).

                      
