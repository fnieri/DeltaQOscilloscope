
-module(wrapper_tcp_server).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, ListenSocket} = gen_tcp:listen(8081, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
    spawn(fun() -> accept_loop(ListenSocket) end),
    {ok, #{socket => ListenSocket}}.

accept_loop(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_client(Socket) end),
    accept_loop(ListenSocket).


handle_client(Socket) ->
     case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            Trimmed = binary:replace(Line, <<"\n">>, <<>>, [global]),
            io:format("~p~n", [Trimmed]),
            otel_wrapper:handle_c_message(Trimmed),
            handle_client(Socket);
        {error, closed} ->
            ok
    end.
