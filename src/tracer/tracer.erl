-module(tracer).
-export([start/1, handler/0]).

start(PIDs) ->
    TracerPid = spawn(fun handler/0),
    lists:foreach(
        fun(Pid) ->
            erlang:trace(Pid, true, [send, 'receive', timestamp])
        end,
        PIDs
    ),
    TracerPid.

handler() ->
    receive
        {trace, Pid, send, Msg, To} ->
            StartNano = erlang:system_time(millisecond),
            io:format("[TRACE] ~p sent ~p to ~p at ~p~n", [Pid, Msg, To, StartNano]),
            handler();

        {trace, Pid, 'receive', Msg} ->
            EndNano = erlang:system_time(millisecond),
            io:format("[TRACE] ~p received ~p at ~p~n", [Pid, Msg, EndNano]),
            handler()
    end.
