
-module(wrapper_tcp_client).
-behaviour(gen_server).

-export([start_link/0, send_span/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {
    socket = undefined,
    logged_disconnected = false
}).

%%% Public API

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

send_span(Data) ->
    gen_server:cast(?SERVER, {send, Data}).

%%% gen_server callbacks

init([]) ->
    State = connect(),
    maybe_schedule_retry(State),
    {ok, State}.

connect() ->
    case gen_tcp:connect("127.0.0.1", 8080, [binary, {active, false}]) of
        {ok, Socket} ->
            io:format("Connected to C++ server on 8080~n"),
            #state{socket = Socket, logged_disconnected = false};
        {error, Reason} ->
            io:format("Connection failed: ~p~n", [Reason]),
            #state{socket = undefined, logged_disconnected = true}
    end.

maybe_schedule_retry(#state{socket = undefined}) ->
    erlang:send_after(5000, self(), retry_connect);
maybe_schedule_retry(_) ->
    ok.

handle_cast({send, Data}, State = #state{socket = undefined, logged_disconnected = false}) ->
    io:format("No socket. Dropping span.~n"),
    maybe_schedule_retry(State),
    {noreply, State#state{logged_disconnected = true}};

handle_cast({send, _Data}, State = #state{socket = undefined}) ->
    %% Already logged, suppress further logs
    maybe_schedule_retry(State),
    {noreply, State};

handle_cast({send, Data}, State = #state{socket = Socket}) ->
    case gen_tcp:send(Socket, Data) of
        ok ->
            {noreply, State};
        {error, Reason} ->
            io:format("TCP send failed: ~p~n", [Reason]),
            NewState = State#state{socket = undefined, logged_disconnected = false},
            maybe_schedule_retry(NewState),
            {noreply, NewState}
    end.

handle_info(retry_connect, State = #state{socket = undefined}) ->
    io:format("Retrying connection to C++ server...~n"),
    NewState = connect(),
    maybe_schedule_retry(NewState),
    {noreply, NewState};

handle_info(_, State) ->
    {noreply, State}.

handle_call(_, _From, State) ->
    {reply, ok, State}.

terminate(_Reason, State) ->
    case State#state.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end.

