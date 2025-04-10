-module(otel_wrapper).
-export([start/0, start_span/1, end_span/2, fail_span/2, span_process/2]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

%% Add TCP client management
start() ->
    tcp_client:start_link().

start_span(Name) ->
    StartTime = erlang:system_time(nanosecond),
    SpanCtx = ?start_span(Name),
    Pid = spawn(?MODULE, span_process, [Name, StartTime]),    
    {Pid, SpanCtx}.

end_span(Pid, SpanCtx) ->
    Pid ! {end_span, erlang:system_time(nanosecond), SpanCtx}.

fail_span(Pid, SpanCtx) ->
    Pid ! {fail_span, a}.

span_process(Name, StartTime) ->
    Timeout = 50,
    Timer = erlang:send_after(Timeout, self(), timeout),
    
    receive
        {fail_span, a} ->
            erlang:cancel_timer(Timer),
            send_span(Name, StartTime, 0, fa); 
        {end_span, EndTime, SpanCtx} ->
            ?end_span(SpanCtx),
            erlang:cancel_timer(Timer),
            send_span(Name, StartTime, EndTime, ok);
        timeout ->
            send_span(Name, StartTime, 0, to)
    end.

send_span(N, B, E, S) ->
    Data = io_lib:format("n:~p;b:~p;e:~p;s:~p~n", [N, B, E, S]),
    tcp_client:send(Data).
