-module(example_2).
-export([start/2, start/3, send/2, worker_loop/4, stop/1, worker_buffer/4]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

start(_Type, _Args) ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    {ok, Pid} = example_sup:start_link(),
    {ok, Pid}.

start(X, Y, K) ->
    Worker1Buffer = spawn_opt(fun() -> worker_buffer(worker_1, undefined, K, 0) end, [{scheduler, 3}]),
    Worker2Buffer = spawn_opt(fun() -> worker_buffer(worker_2, undefined, K, 0) end, [{scheduler, 4}]),
    Worker2 = spawn_opt(fun() -> worker_loop(worker_2, Y,  Worker1Buffer, Worker2Buffer) end, [{scheduler, 1}]),
    Worker1 = spawn_opt(fun() -> worker_loop(worker_1, Y,  Worker1Buffer, Worker2Buffer) end, [{scheduler, 2}]),


    Worker1Buffer ! {set_worker, Worker1},
    Worker2Buffer ! {set_worker, Worker2},

    Sender = spawn(fun() -> send(X, Worker1Buffer) end),

    {ok, self()}.

send(X, Worker1Buffer) ->
Delay = -math:log(rand:uniform()) / X, 
    timer:sleep(trunc(Delay * 1000)),
    {Pid, SpanCtx} = otel_wrapper:start_span(probe),

    Worker1Buffer ! {Pid, SpanCtx} ,
        send(X, Worker1Buffer).

worker_buffer(worker_1, Worker1, K, QueueLength) ->
    receive
        
        {set_worker, NewWorker} ->
            worker_buffer(worker_1, NewWorker, K, QueueLength);
        
        {Pid, ProbeCtx} when QueueLength < K ->
            %WorkerSpan = ?start_span(worker_1),
            {WorkerPid, WorkerSpan} = otel_wrapper:start_span(worker_1),
            Worker1 ! {Pid, WorkerPid, ProbeCtx, WorkerSpan},

            worker_buffer(worker_1, Worker1, K, QueueLength + 1);
        
        {done} ->
            worker_buffer(worker_1, Worker1, K, QueueLength - 1);
        
        {} when QueueLength >= K ->
%            ?with_span(<<"buffer_1">>, #{}, fun(_ChildSpanCtx) ->
 %               otel_span:set_status(_ChildSpanCtx, opentelemetry:status(error, <<"Queue full">>))
  %                                          end),
            worker_buffer(worker_1, Worker1, K, QueueLength)
           end;

worker_buffer(worker_2, Worker2, K, QueueLength) ->
    receive
        
        {set_worker, NewWorker} ->
            worker_buffer(worker_2, NewWorker, K, QueueLength);
        
        {Pid, ProbeCtx} when QueueLength < K ->
            {WorkerPid, WorkerSpan} =  otel_wrapper:start_span(worker_2),
            Worker2 ! {Pid, WorkerPid, ProbeCtx, WorkerSpan},
            worker_buffer(worker_2, Worker2, K, QueueLength + 1);
        
        {done} ->
            worker_buffer(worker_2, Worker2, K, QueueLength - 1);

        {} when QueueLength >= K ->
            %error
            worker_buffer(worker_2, Worker2, K, QueueLength)
    end.

worker_loop(worker_1, Y, Worker1Buffer, Worker2Buffer) ->
    receive
        {Pid, WorkerPid, ProbeCtx, WorkerSpan} ->
proc_lib:spawn_link(fun() ->
                        otel_ctx:attach(ProbeCtx),
                        ?set_current_span(WorkerSpan),
                                 Loops = rand:uniform(Y),
                                loop(Loops),
                                Worker2Buffer ! {Pid, ProbeCtx},
                        otel_wrapper:end_span(WorkerPid, WorkerSpan),
                        ?set_current_span(ProbeCtx)
                    end),
                                   
            Worker1Buffer ! {done},
            worker_loop(worker_1, Y,  Worker1Buffer, Worker2Buffer)
    end;

worker_loop(worker_2, Y, Worker1Buffer, Worker2Buffer) ->
    receive
        {Pid, WorkerPid, ProbeCtx, WorkerSpan} ->
            proc_lib:spawn_link(fun() ->
                        otel_ctx:attach(ProbeCtx),
                        ?set_current_span(WorkerSpan),
                        Loops = rand:uniform(Y),
                        loop(Loops),
                    %    timer:sleep(1000), 
                       Prob = rand:uniform(),
                        if Prob > 0.5 -> 
                            timer:sleep(1000);
                        true ->
                            ok
                        end,
                        otel_wrapper:end_span(WorkerPid,WorkerSpan),
                        ?set_current_span(ProbeCtx),
                       
                        otel_wrapper:end_span(Pid, ProbeCtx)
                    end),
           
                Worker2Buffer ! {done},   
            worker_loop(worker_2, Y,  Worker1Buffer, Worker2Buffer)
    end.

loop(0) -> ok;
loop(N) -> loop(N - 1).

stop(_State) ->
    ok.
