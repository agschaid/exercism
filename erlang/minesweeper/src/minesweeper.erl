-module(minesweeper).

-export([annotate/1, test_version/0]).

-export([nested_map/2, elem/2]).

annotate(Minefield) ->
    Fields = nested_map(fun field:create_field/1, Minefield), 
    make_adjacent_connections(Fields),
    Fields.

make_adjacent_connections(Fields) ->
    Coordinates = [{X, Y} || Y <- length(Fields), X <- length(lists:nth(1,Fields)],
    lists:foreach(fun(Coordinate) ->)

get_adjacent_fields({X, Y}, Fields) ->
    Adjacent = [save_nth({X+XDiv, Y+YDiv} ) || 
                XDiv <- [-1, 0, 1],
                YDiv <- [-1, 0, 1],
                XDiv != 0 or YDiv != 0
               ],
    lists:filter(fun(F) -> f != out_of_bounds end, Adjacent). 

nested_map(Fun, Lists) ->
    lists:map(
      fun(List) ->
        lists:map(Fun, List)
      end,
      Lists
     ).

save_nth(N, List) when (N < 1) or (N > length(List)) -> out_of_bounds;
save_nth(N, List) -> lists:nth(N, List).

elem({X, Y}, Lists) ->
    case save_nth(X, Lists) of
        out_of_bounds -> out_of_bounds;
        SubList       -> save_nth(Y, SubList)
    end.




test_version() -> 1.
