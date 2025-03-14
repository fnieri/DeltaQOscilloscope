-module(example).
-export([start/2, start/3, send/2, worker_loop/4, stop/1, worker_buffer/4]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

start(_Type, _Args) ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    {ok, Pid} = example_sup:start_link(),
    {ok, Pid}.

start(X, Y, K) ->
    %Worker1Buffer = spawn_opt(fun() -> worker_buffer(worker_1, undefined, K, 0) end, [{scheduler, 3}]),
    %Worker2Buffer = spawn_opt(fun() -> worker_buffer(worker_2, undefined, K, 0) end, [{scheduler, 4}]),
Worker1Buffer = spawn(fun() -> worker_buffer(worker_1, undefined, K, 0) end),
Worker2Buffer = spawn(fun() -> worker_buffer(worker_2, undefined, K, 0) end),
    %Worker2 = spawn_opt(fun() -> worker_loop(worker_2, Y,  Worker1Buffer, Worker2Buffer) end, [{scheduler, 1}]),
    %Worker1 = spawn_opt(fun() -> worker_loop(worker_1, Y,  Worker1Buffer, Worker2Buffer) end, [{scheduler, 2}]),
    Worker2 = spawn(fun() -> worker_loop(worker_2, Y,  Worker1Buffer, Worker2Buffer) end),
    Worker1 = spawn(fun() -> worker_loop(worker_1, Y,  Worker1Buffer, Worker2Buffer) end),


    Worker1Buffer ! {set_worker, Worker1},
    Worker2Buffer ! {set_worker, Worker2},

    Sender = spawn(fun() -> send(X, Worker1Buffer) end),

    {ok, self()}.

send(X, Worker1Buffer) ->
    
    Delay = -math:log(rand:uniform()) / X, 
    timer:sleep(trunc(Delay * 1000)),
    Worker1Buffer ! {},
    send(X, Worker1Buffer).

worker_buffer(worker_1, Worker1, K, QueueLength) ->
    receive
        
        {set_worker, NewWorker} ->
            worker_buffer(worker_1, NewWorker, K, QueueLength);
        
        {} when QueueLength < K ->
            Worker1 ! {},
            worker_buffer(worker_1, Worker1, K, QueueLength + 1);
        
        {processed} ->
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
        
        {} when QueueLength < K ->
            Worker2 ! {},
            worker_buffer(worker_2, Worker2, K, QueueLength + 1);
        
        {processed} ->
            worker_buffer(worker_2, Worker2, K, QueueLength - 1);

        {} when QueueLength >= K ->
            %error
            worker_buffer(worker_2, Worker2, K, QueueLength)
    end.

worker_loop(worker_1, Y, Worker1Buffer, Worker2Buffer) ->
    receive
        {} ->
            Loops = rand:uniform(Y),
            loop(Loops),
            Worker2Buffer ! {},
            Worker1Buffer ! {processed},
                                                     
            worker_loop(worker_1, Y,  Worker1Buffer, Worker2Buffer)
    end;

worker_loop(worker_2, Y, Worker1Buffer, Worker2Buffer) ->
    receive
        {} ->
            Loops = rand:uniform(Y),
            loop(Loops),
            Worker2Buffer ! {processed},            
            worker_loop(worker_2, Y,  Worker1Buffer, Worker2Buffer)
    end.

loop(0) -> ok;
loop(N) -> loop(N - 1).

stop(_State) ->
    ok.
