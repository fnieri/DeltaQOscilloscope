-module(example_2).
-export([start/0, my_function/0]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

start() ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    
    % Start the root span
    ?with_span(<<"root_span">>, #{}, fun(_SpanCtx) ->
        % Capture the current context
        
         
        % Simulate some work in the root span
        io:format("Root span is doing some work~n"),
        timer:sleep(1000) % Simulate work
    end).

my_function() ->
    % Attach the parent context in the child process
   Ctx = otel_ctx:get_current(),
    otel_ctx:attach(Ctx),
    
    % Start a child span under the root span
    ?with_span(<<"child_span">>, #{}, fun(_ChildSpanCtx) ->
        % Simulate some work in the child span
        io:format("Child span is doing some work~n"),
        timer:sleep(500) % Simulate work
    end).
