
-module(otel_wrapper_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ets:new(timeout_registry, [named_table, public, set]),
    %% Start TCP server as a child
    ChildSpecs = [
        #{id => tcp_server,
          start => {otel_wrapper, start_tcp_server, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [otel_wrapper]}
    ],
    {ok, {{one_for_one, 1, 5}, ChildSpecs}}.
