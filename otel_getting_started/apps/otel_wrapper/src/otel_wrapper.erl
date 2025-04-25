-module(otel_wrapper).
-behaviour(application).

-export([start/0, start_span/1, end_span/1, fail_span/1, span_process/2]).
-export([start/2, stop/1]).
-export([connection_manager/1]).

%%%=======================
%%% Application Callbacks
%%%=======================

start(_Type, _Args) ->
    ets:new(timeout_registry, [named_table, public, set]),
    ets:new(otel_connections, [named_table, public, set]),
    ConnectionPid = spawn_link(?MODULE, connection_manager, [undefined]),
    register(otel_connection_manager, ConnectionPid),
    spawn(fun start_tcp_server/0),
    {ok, self()}.

stop(_State) ->
    case whereis(otel_connection_manager) of
        undefined -> ok;
        Pid -> exit(Pid, normal)
    end,
    ok.

%%%=======================
%%% Public API
%%%=======================

start() ->
    application:ensure_all_started(otel_wrapper).

start_span(Name) ->
    StartTime = erlang:system_time(nanosecond),
    Pid = spawn(?MODULE, span_process, [Name, StartTime]),
    Pid.

end_span(Pid) ->
    Pid ! {end_span, erlang:system_time(nanosecond)}.

fail_span(Pid) ->
    Pid ! {fail_span, a, b}.

%%%=======================
%%% Span Worker
%%%=======================

span_process(NameBin, StartTime) ->
    Timeout = case ets:lookup(timeout_registry, NameBin) of
        [{_, T}] -> T;
        [] -> 50
    end,
    Timer = erlang:send_after(Timeout, self(), timeout),

    receive
        {fail_span, a, b} ->
            erlang:cancel_timer(Timer),
            send_span(NameBin, StartTime, 0, <<"fa">>);
        {end_span, EndTime} ->
            erlang:cancel_timer(Timer),
            send_span(NameBin, StartTime, EndTime, <<"ok">>);
        timeout ->
            send_span(NameBin, StartTime, 0, <<"to">>)
    end.

%%%=======================
%%% TCP Server to Receive Commands from C
%%%=======================

start_tcp_server() ->
    {ok, ListenSocket} = gen_tcp:listen(8081, [binary, {packet, line}, {active, false}, {reuseaddr, true}]),
    accept(ListenSocket).

accept(ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(fun() -> handle_connection(Socket) end),
    accept(ListenSocket).

handle_connection(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Line} ->
            io:format("~p~n", [Line]),
            Trimmed = binary:replace(Line, <<"\n">>, <<>>, [global]),
            io:format("~p~n", [Trimmed]),
            handle_c_message(Trimmed),
            handle_connection(Socket);
        {error, closed} ->
            ok
    end.

%%%=======================
%%% Handle Incoming Messages from C
%%%=======================

handle_c_message(Bin) when is_binary(Bin) ->
    case binary:split(Bin, <<";">>, [global]) of
        [<<"set_timeout">>, Name, TimeoutBin] ->
            case string:to_integer(binary_to_list(TimeoutBin)) of
                {Timeout, _} when is_integer(Timeout) ->
                    ets:insert(timeout_registry, {Name, Timeout}),
                    io:format("Timeout set: ~p = ~p~n", [Name, Timeout]);
                _ ->
                    io:format("Invalid timeout: ~p~n", [TimeoutBin])
            end;
        _ ->
            io:format("Unknown command: ~p~n", [Bin])
    end.

%%%=======================
%%% Connection Manager
%%%=======================

connection_manager(Socket) ->
    receive
        {get_connection, From} ->
            case ensure_connection(Socket) of
                {ok, NewSocket} ->
                    From ! {connection, NewSocket},
                    connection_manager(NewSocket);
                {error, _Reason} ->
                    From ! {connection, undefined},
                    connection_manager(undefined)
            end;
        {release_connection, error} ->
            case Socket of
                undefined -> ok;
                _ -> gen_tcp:close(Socket)
            end,
            connection_manager(undefined);
        shutdown ->
            case Socket of
                undefined -> ok;
                _ -> gen_tcp:close(Socket)
            end,
            exit(normal)
    end.

ensure_connection(Socket) ->
    case Socket of
        undefined ->
            connect_to_c();
        _ ->
            case gen_tcp:send(Socket, <<>>) of
                ok -> {ok, Socket};
                {error, _} -> 
                    gen_tcp:close(Socket),
                    connect_to_c()
            end
    end.

get_connection() ->
    otel_connection_manager ! {get_connection, self()},
    receive
        {connection, Socket} -> Socket
    after 1000 ->
        undefined
    end.

%%%=======================
%%% Sending Span Data to C
%%%=======================

send_span(NameBin, Start, End, StatusBin) ->
    Data = io_lib:format("n:~s;b:~p;e:~p;s:~s~n", [
        NameBin,
        Start,
        End,
        StatusBin
    ]),
    case get_connection() of
        undefined ->
            io:format("Failed to get connection to C server~n");
        Socket ->
            case gen_tcp:send(Socket, lists:flatten(Data)) of
                ok -> ok;
                {error, Reason} ->
                    io:format("Error sending span: ~p~n", [Reason]),
                    otel_connection_manager ! {release_connection, error}
            end
    end.

connect_to_c() ->
    case gen_tcp:connect("127.0.0.1", 8080, [binary, {packet, 0}, {active, false}], 1000) of
        {ok, Socket} -> {ok, Socket};
        {error, Reason} ->
            {error, Reason}
    end.
