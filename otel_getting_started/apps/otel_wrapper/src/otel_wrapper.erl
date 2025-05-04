
-module(otel_wrapper).
-behaviour(application).

-export([start/0, start_span/1,  end_span/2, fail_span/1, with_span/2, span_process/3]).
-export([start/2, stop/1]).
-export([start_tcp_server/0, connection_manager/1]).
-export([init_ets/0, start_connection_manager/0]).
-export([set_stub_running/1]).


-include_lib("opentelemetry_api/include/otel_tracer.hrl").


%% @moduledoc
%% `otel_wrapper` is an Erlang module built on top of OpenTelemetry
%% to pair with the DeltaQ osciloscope. It tracks spans start, end, uses a custom timeout defined by the user in the oscilloscope,
%% and allows runtime configuration through TCP.
%%
%% Features:
%% - Span lifecycle management (start/end/fail/timeout)
%% - Dynamic timeouts for spans
%% - Supports toggling stub behavior at runtime
%%
%% Usage:
%%   otel_wrapper:start().
%%   {Ctx, Pid} = otel_wrapper:start_span(<<"my_span">>).
%%   otel_wrapper:end_span(Ctx, Pid).
%%   otel_wrapper:fail_span(Pid)


%%%=======================
%%% Application Callbacks
%%%=======================


start(_Type, _Args) ->
    otel_wrapper_sup:start_link().

stop(_State) ->
    ok.

init_ets() ->
    ets:new(timeout_registry, [named_table, public, set]),
    ets:new(otel_connections, [named_table, public, set]),
    ets:new(otel_state, [named_table, public, set]),
    ets:insert(otel_state, {stub_running, false}),
    {ok, self()}.

start_connection_manager() ->
    ConnectionPid = spawn_link(?MODULE, connection_manager, [undefined]),
    register(otel_connection_manager, ConnectionPid),
    {ok, ConnectionPid}.


%%%=======================
%%% For testing purposes
%%% ======================

set_stub_running(Bool) when is_boolean(Bool) ->
    ets:insert(otel_state, {stub_running, Bool}),
    io:format("Stub running set to: ~p~n", [Bool]),
    ok.

%%%=======================
%%% Public API
%%%=======================
%% @doc Starts the otel_wrapper application and all dependencies.
-spec start() -> {ok, [atom()]} | {error, term()}.
start() ->
    application:ensure_all_started(otel_wrapper).

%% @doc Starts a span with the given name, if the stub is running.
%% Returns a tuple of SpanContext and the internal span process PID or `ignore`.
-spec start_span(binary()) -> {opentelemetry:span_ctx(), pid() | ignore}.
start_span(Name) ->
    SpanCtx = ?start_span(Name),
    case ets:lookup(otel_state, stub_running) of
        [{_, true}] ->
            case ets:lookup(timeout_registry, Name) of
                [{_, T}] -> 
                    StartTime = erlang:system_time(nanosecond),
                    Pid = spawn(?MODULE, span_process, [Name, StartTime, T]),
                    {SpanCtx, Pid};
                [] ->
                    {SpanCtx, ignore}
            end;
        _ -> 
            {SpanCtx, ignore}
    end.

start_span(SpanCtx, Name) ->
    case ets:lookup(otel_state, stub_running) of
        [{_, true}] ->
            case ets:lookup(timeout_registry, Name) of
                [{_, T}] -> 
                    StartTime = erlang:system_time(nanosecond),
                    Pid = spawn(?MODULE, span_process, [Name, StartTime, T]),
                    {SpanCtx, Pid};
                [] ->
                    {SpanCtx, ignore}
            end;
        _ -> 
            {SpanCtx, ignore}
    end.


%% @doc Ends the span and reports it, unless stub is disabled or Pid is `ignore`.
-spec end_span(opentelemetry:span_ctx(), pid() | ignore) -> ok | term().
end_span(Ctx, Pid) ->
    ?end_span(Ctx),
        case Pid of
            ignore -> ok;
        _ when is_pid(Pid) ->
            Pid ! {end_span, erlang:system_time(nanosecond)}
    end.


%% @doc Fail the span and reports it to the oscilloscope, unless stub is disabled or Pid is `ignore`.
-spec fail_span( pid() | ignore) -> ok | term().
fail_span(Pid) ->
    case Pid of
        ignore -> ok;
    _ when is_pid(Pid) ->
        Pid ! fail_span
    end.


-spec with_span(binary(), fun(() -> any())) -> any().
with_span(Name, Fun) ->
    ?with_span(Name, #{}, 
        fun(SpanCtx) ->
            Pid = start_with_span(Name),
                Result = Fun(),
                end_with_span(Pid),
                Result
        end).

start_with_span(Name) ->
    case ets:lookup(otel_state, stub_running) of
        [{_, true}] ->
            case ets:lookup(timeout_registry, Name) of
                [{_, T}] -> 
                    StartTime = erlang:system_time(nanosecond),
                    Pid = spawn(?MODULE, span_process, [Name, StartTime, T]),
                    Pid;
                [] ->
                    ignore
            end;
        _ -> 
            ignore
    end.

end_with_span(Pid) ->
        case Pid of
            ignore -> ok;
        _ when is_pid(Pid) ->
            Pid ! {end_span, erlang:system_time(nanosecond)}
    end.



%%%=======================
%%% Span Worker
%%%=======================

span_process(NameBin, StartTime, Timeout) ->
    Timer = erlang:send_after(Timeout, self(), timeout),
    receive
        fail_span ->
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
         [<<"start_stub">>] ->
            ets:insert(otel_state, {stub_running, true}),
            io:format("Stub enabled~n");
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
        ets:insert(otel_state, {stub_running, false}),
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
