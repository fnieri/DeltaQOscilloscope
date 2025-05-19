
-module(otel_wrapper_sup).
-behaviour(supervisor).
-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
      %  Host = application:get_env(otel_wrapper, client_host, {127,0,0,1}),
      %  ClientPort = application:get_env(otel_wrapper, client_port, 8080),
      %  ServerPort = application:get_env(otel_wrapper, server_port, 8081),
      %  ServerIP = application:get_env(otel_wrapper, server_ip, {0,0,0,0}),
    
        #{id => ets_init,
          start => {otel_wrapper, init_ets, []},
          restart => temporary,
          shutdown => brutal_kill,
          type => worker,
          modules => [otel_wrapper]},
          
        #{id => tcp_server,
          start => {wrapper_tcp_server, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [wrapper_tcp_server]},
          
        #{id => tcp_client,
          start => {wrapper_tcp_client, start_link, []},
          restart => permanent,
          shutdown => brutal_kill,
          type => worker,
          modules => [wrapper_tcp_client]}
    ],
    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.

