-module(tcp_client).
-export([start_link/0, send/1]).

start_link() ->
    Pid = spawn_link(fun init/0),
    register(?MODULE, Pid),
    {ok, Pid}.

init() ->
    {ok, Socket} = gen_tcp:connect("localhost", 8080, 
        [binary, {active, false}, {keepalive, true}]),
    loop(Socket).

loop(Socket) ->
    receive
        {send, Data} ->
            gen_tcp:send(Socket, Data),
            loop(Socket);
        _ ->
            loop(Socket)
    end.

send(Data) ->
    ?MODULE ! {send, Data}.
