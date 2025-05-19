
-module(otel_wrapper_udp).
-behaviour(application).
-author("Francesco Nieri").

-export([start/0, start_span/1, start_span/2,  end_span/2, fail_span/1, with_span/2, with_span/3, span_process/3]).
-export([start/2, stop/1]).
-export([init_ets/0]).
-export([set_stub_running/1]).
-export([start_tcp_server/0]).

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
    %start_udp_sender(),
    {ok, self()}.


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

%% @doc Starts a span with attributes.
-spec start_span(binary(), map()) -> {opentelemetry:span_ctx(), pid() | ignore}.
start_span(Name, Attrs) when is_map(Attrs) ->
    SpanCtx = ?start_span(Name, Attrs),
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



%% @doc Executes Fun inside a span with attributes.
-spec with_span(binary(), fun(() -> any())) -> any().
with_span(Name, Fun) ->
    ?with_span(Name, #{}, 
        fun(SpanCtx) ->
            Pid = start_with_span(Name),
            Result = Fun(),
            end_with_span(Pid),
            Result
        end).

%% @doc Executes Fun inside a span with attributes.
-spec with_span(binary(), fun(() -> any()), map()) -> any().
with_span(Name, Fun, Attrs) when is_map(Attrs), is_function(Fun, 0) ->
    ?with_span(Name, Attrs,
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
%%% Init UDP
%%%=======================

start_udp_sender() ->
    case gen_udp:open(0, [binary, {active, true}]) of
        {ok, Socket} ->
            ets:insert(otel_state, {udp_socket, Socket}),
            ets:insert(otel_state, {socket_error, false}),
            {ok, Socket};
        Error ->
            io:format("Error ~p", [Error]),
            Error
    end.

get_udp_socket() ->
    case ets:lookup(otel_state, udp_socket) of
        [{_, Socket}] ->
            % Check if socket is still valid only when we have a previous error
            case ets:lookup(otel_state, socket_error) of
                [{_, true}] ->
                    case inet:port(Socket) of
                        {ok, _} -> 
                            ets:insert(otel_state, {socket_error, false}),
                            Socket;
                        {error, _} -> 
                            %gen_udp:close(Socket),
                            case start_udp_sender() of
                                {ok, NewSocket} -> 
                                    ets:insert(otel_state, {socket_error, false}),
                                    NewSocket;
                                _ -> undefined
                            end
                    end;
                _ ->
                    Socket
            end;
        [] ->
            case start_udp_sender() of
                {ok, Socket} -> 
                    ets:insert(otel_state, {socket_error, false}),
                    Socket;
                _ -> undefined
            end
    end.

%%%=======================
%%% Sending Span Data to C
%%%=======================

send_span(NameBin, Start, End, StatusBin) ->
    Data = io_lib:format("n:~s;b:~p;e:~p;s:~s", [
        NameBin,
        Start,
        End,
        StatusBin
    ]),
    Packet = list_to_binary(lists:flatten(Data)),
    case get_udp_socket() of
        undefined ->
            ok;
        Socket ->
            case gen_udp:send(Socket, "127.0.0.1", 8080, Packet) of
                ok -> 
                    ets:insert(otel_state, {socket_error, false}),
                    ok;
                {error, Reason} ->
                    io:format("~p", [Reason]),
                    ets:insert(otel_state, {socket_error, true})
            end
    end.
