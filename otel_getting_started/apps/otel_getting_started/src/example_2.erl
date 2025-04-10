-module(example_2).
-export([start/2, start/3, send/2, worker_loop/4, stop/1, worker_buffer/4, notify_queue_resize/2]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

start(_Type, _Args) ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    {ok, Pid} = example_sup:start_link(),
    {ok, Pid}.

controller(Load, Loops, QueueSize) ->
    receive
        {set_load, NewLoad} ->
            io:format("Updating load to ~p~n", [NewLoad]),
            controller(NewLoad, Loops, QueueSize);

        {set_loops, NewLoops} ->
            io:format("Updating loops to ~p~n", [NewLoops]),
            controller(Load, NewLoops, QueueSize);

        {set_queue_size, NewK} ->
            io:format("Updating queue size to ~p~n", [NewK]),
            % notify buffers
            example_2:notify_queue_resize(worker_1, NewK),
            example_2:notify_queue_resize(worker_2, NewK),
            controller(Load, Loops, NewK);

        {get_config, From} ->
            From ! {config, Load, Loops, QueueSize},
            controller(Load, Loops, QueueSize)
    end.

start(X, Y, K) ->
    ControllerPid = spawn(fun() -> controller(X, Y, K) end),
    Worker1Buffer = spawn_opt(fun() -> worker_buffer(worker_1, undefined, K, 0) end, [{scheduler, 3}]),
    Worker2Buffer = spawn_opt(fun() -> worker_buffer(worker_2, undefined, K, 0) end, [{scheduler, 4}]),
    Worker2 = spawn_opt(fun() -> worker_loop(worker_2, Y,  Worker1Buffer, Worker2Buffer) end, [{scheduler, 1}]),
    Worker1 = spawn_opt(fun() -> worker_loop(worker_1, Y,  Worker1Buffer, Worker2Buffer) end, [{scheduler, 2}]),


    Worker1Buffer ! {set_worker, Worker1},
    Worker2Buffer ! {set_worker, Worker2},

    register(worker_1, Worker1Buffer),
    register(worker_2, Worker2Buffer),

    spawn(fun() -> send(ControllerPid, Worker1Buffer) end),
    register(example_controller, ControllerPid),
    {ok, self()}.

notify_queue_resize(WorkerName, NewK) ->
    case whereis(WorkerName) of
        undefined -> ok;
        Pid -> Pid ! {update_k, NewK}
    end.


send(ControllerPid, Worker1Buffer) ->
    ControllerPid ! {get_config, self()},
    receive
        {config, Load, _, _} ->
            Delay = -math:log(rand:uniform()) / Load,
            timer:sleep(trunc(Delay * 1000)),
            {Pid, SpanCtx} = otel_wrapper:start_span(probe),
            Worker1Buffer ! {Pid, SpanCtx},
            send(ControllerPid, Worker1Buffer)
    end.


worker_buffer(worker_1, Worker1, K, QueueLength) ->
    receive
        {update_k, NewK} ->   
            worker_buffer(worker_1, Worker1, NewK, QueueLength);

        {set_worker, NewWorker} ->
            worker_buffer(worker_1, NewWorker, K, QueueLength);
        
        {Pid, ProbeCtx} when QueueLength < K ->
            %WorkerSpan = ?start_span(worker_1),
            {WorkerPid, WorkerSpan} = otel_wrapper:start_span(worker_1),
            Worker1 ! {Pid, WorkerPid, ProbeCtx, WorkerSpan},

            worker_buffer(worker_1, Worker1, K, QueueLength + 1);
        
        {done} ->
            worker_buffer(worker_1, Worker1, K, QueueLength - 1);
        
        {Pid, ProbeCtx} when QueueLength >= K ->
            {WorkerPid, WorkerSpan} = otel_wrapper:start_span(worker_1),
            otel_wrapper:fail_span(WorkerPid, WorkerSpan),
            otel_wrapper:fail_span(Pid, ProbeCtx),
            worker_buffer(worker_1, Worker1, K, QueueLength)
           end;

worker_buffer(worker_2, Worker2, K, QueueLength) ->
    receive
        {update_k, NewK} ->
            worker_buffer(worker_2, Worker2, NewK, QueueLength);
        {set_worker, NewWorker} ->
            worker_buffer(worker_2, NewWorker, K, QueueLength);
        
        {Pid, ProbeCtx} when QueueLength < K ->
            {WorkerPid, WorkerSpan} =  otel_wrapper:start_span(worker_2),
            Worker2 ! {Pid, WorkerPid, ProbeCtx, WorkerSpan},
            worker_buffer(worker_2, Worker2, K, QueueLength + 1);
        
        {done} ->
            worker_buffer(worker_2, Worker2, K, QueueLength - 1);

        {Pid, ProbeCtx} when QueueLength >= K ->
            {WorkerPid, WorkerSpan} = otel_wrapper:start_span(worker_1),
            otel_wrapper:fail_span(WorkerPid, WorkerSpan),
            otel_wrapper:fail_span(Pid, ProbeCtx),
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
                   %    Prob = rand:uniform(),
                   %     if Prob > 0.5 -> 
                   %         timer:sleep(10);
                   %     true ->
                   %         ok
                   %     end,
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
