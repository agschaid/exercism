-module(field).
-behaviour(gen_server).

-export([create_field/1]).
-export([adjacent/2, expand_bomb/1, char/1]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([code_change/3, handle_info/2, terminate/2]).

create_field(Char) ->
    {ok, Field} = gen_server:start_link(?MODULE, Char, []),
    Field.

adjacent(This, Adjacent) ->
    gen_server:cast(This, {adjacent, Adjacent}).

expand_bomb(This) ->
    gen_server:cast(This, expand_bomb).

char(This) ->
    gen_server:call(This, char).


init('*') -> {ok, {bomb, []}};
init(' ') -> {ok, {0, []}}.

handle_cast({adjacent, NewAdj}, {FieldState, Adjs}) ->
    {noreply, {FieldState, [NewAdj | Adjs]}};

handle_cast(expand_bomb, {bomb, Adjs}) ->
    lists:foreach( fun(Adj) -> gen_server:cast(Adj, increase_counter) end
                   , Adjs),
    {noreply, {bomb, Adjs}};

handle_cast(expand_bomb, State) ->
    {noreply, State};


handle_cast(increase_counter, {bomb, Adjs}) ->
    {noreply, {bomb, Adjs}};

handle_cast(increase_counter, {N, Adjs}) ->
    {noreply, {N+1, Adjs}}.

handle_call(char, _From, {bomb, Adjs}) ->
    {reply, '*', {bomb, Adjs}};

handle_call(char, _From, {0, Adjs}) ->
    {reply, ' ', {0, Adjs}};

handle_call(char, _From, {N, Adjs}) ->
    Char = lists:nth(1, integer_to_list(N)),
    {reply, Char, {N, Adjs}}.

code_change(_A, _B, _C) -> undefined.
handle_info(_A, _B) -> undefined.
terminate(_Reason, _State) -> io:format("~p~n", ["See ya"]).


