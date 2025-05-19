
-module(wrapper_tcp_client).
-behaviour(gen_server).

-export([start_link/0, send_span/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

-define(SERVER, ?MODULE).

-record(state, {socket = undefined}).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% Public API
send_span(Data) ->
    gen_server:cast(?SERVER, {send, Data}).

init([]) ->
    {ok, connect(), 0}.

connect() ->
    case gen_tcp:connect("127.0.0.1", 8080, [binary, {active, false}]) of
        {ok, Socket} -> #state{socket = Socket};
        {error, _} -> #state{socket = undefined}
    end.

handle_cast({send, Data}, State = #state{socket = undefined}) ->
    io:format("No socket. Dropping span: ~p~n", [Data]),
    {noreply, State};

handle_cast({send, Data}, State = #state{socket = Socket}) ->
    case gen_tcp:send(Socket, Data) of
        ok -> {noreply, State};
        {error, Reason} ->
            io:format("TCP send failed: ~p~n", [Reason]),
            {noreply, State#state{socket = undefined}}
    end.

handle_info(_, State) -> {noreply, State}.
handle_call(_, _From, State) -> {reply, ok, State}.
terminate(_, State) ->
    case State#state.socket of
        undefined -> ok;
        Socket -> gen_tcp:close(Socket)
    end.
