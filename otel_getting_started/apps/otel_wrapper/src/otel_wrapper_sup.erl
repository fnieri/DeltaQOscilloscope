-module(otel_wrapper_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        #{id => ets_init,
          start => {otel_wrapper, init_ets, []},
          restart => temporary,
          shutdown => brutal_kill,
          type => worker,
          modules => [otel_wrapper]},
        
        #{id => connection_manager,
          start => {otel_wrapper, start_connection_manager, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [otel_wrapper]},
        
        #{id => tcp_server,
          start => {otel_wrapper, start_tcp_server, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [otel_wrapper]}
    ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

