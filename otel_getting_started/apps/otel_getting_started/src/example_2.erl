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

    register(worker_1, Worker1Buffer),
    register(worker_2, Worker2Buffer),
    spawn(fun() -> send(X, Worker1Buffer) end),

      {ok, self()}.

send(X, Worker1Buffer) ->
    Delay = -math:log(rand:uniform()) / X,
    timer:sleep(trunc(Delay * 1000)),
   
    B = maps:new(),
    {ProbeCtx, Pid} = otel_wrapper:start_span(<<"probe">>),
    B1 = maps:put(<<"probe_id">>, Pid, B),

    Baggage = maps:put(<<"probe_ctx">>, ProbeCtx, B1),
    
    Worker1Buffer ! Baggage,
    send(X, Worker1Buffer).

worker_buffer(worker_1, Worker1, K, QueueLength) ->
    receive
        {update_k, NewK} ->   
            worker_buffer(worker_1, Worker1, NewK, QueueLength);

        {set_worker, NewWorker} ->
            worker_buffer(worker_1, NewWorker, K, QueueLength);
        
        #{<<"probe_ctx">> := _} = CtxBaggage when QueueLength < K ->
            {WorkerCtx, WorkerPid} = otel_wrapper:start_span(<<"worker_1">>),
            
            B = maps:put(<<"worker_id">>, WorkerPid, CtxBaggage),
            Baggage = maps:put(<<"worker_ctx">>, WorkerCtx, B),
            
            Worker1 ! Baggage,
            worker_buffer(worker_1, Worker1, K, QueueLength + 1);
        
        done ->  % Changed to atom instead of binary
            worker_buffer(worker_1, Worker1, K, QueueLength - 1);
        
        #{<<"probe_ctx">> := _} = CtxBaggage when QueueLength >= K ->
            {WorkerCtx, WorkerPid} = otel_wrapper:start_span(<<"worker_1">>),
            Pid = maps:get(<<"probe_id">>, CtxBaggage, undefined), 
            
            otel_wrapper:fail_span(WorkerPid),
            otel_wrapper:fail_span(Pid),
            
            worker_buffer(worker_1, Worker1, K, QueueLength)
    end;

worker_buffer(worker_2, Worker2, K, QueueLength) ->
    receive
        {update_k, NewK} ->
            worker_buffer(worker_2, Worker2, NewK, QueueLength);
        {set_worker, NewWorker} ->
            worker_buffer(worker_2, NewWorker, K, QueueLength);
        
        #{<<"probe_ctx">> := _} = CtxBaggage when QueueLength < K ->
            {WorkerCtx, WorkerPid} = otel_wrapper:start_span(<<"worker_2">>),
            
            B = maps:put(<<"worker_id">>, WorkerPid, CtxBaggage),
            Baggage = maps:put(<<"worker_ctx">>, WorkerCtx, B),
            
            Worker2 ! Baggage,
            worker_buffer(worker_2, Worker2, K, QueueLength + 1);

        done ->  % Changed to atom
            worker_buffer(worker_2, Worker2, K, QueueLength - 1);

        #{<<"probe_ctx">> := _} = ProbeCtx when QueueLength >= K ->
            {WorkerCtx, WorkerPid} = otel_wrapper:start_span(<<"worker_1">>),
            Pid = maps:get(<<"probe_id">>, ProbeCtx, undefined),
            otel_wrapper:fail_span(WorkerPid),
            otel_wrapper:fail_span(Pid),
            worker_buffer(worker_2, Worker2, K, QueueLength)
    end.

worker_loop(worker_1, Y, Worker1Buffer, Worker2Buffer) ->
    receive
        CtxBaggage ->
            ProbeCtx = maps:get(<<"probe_ctx">>, CtxBaggage),
            WorkerCtx = maps:get(<<"worker_ctx">>, CtxBaggage),
            WorkerPid = maps:get(<<"worker_id">>, CtxBaggage),
            proc_lib:spawn_link(fun() ->
                        otel_ctx:attach(ProbeCtx),
                        ?set_current_span(WorkerCtx),
                                Loops = rand:uniform(Y),
                                loop(Loops),
                                Worker2Buffer ! CtxBaggage,
                        otel_wrapper:end_span(WorkerCtx, WorkerPid),
                        ?set_current_span(ProbeCtx)
                    end),
                                   
            Worker1Buffer ! done,
            worker_loop(worker_1, Y,  Worker1Buffer, Worker2Buffer)
    end;

worker_loop(worker_2, Y, Worker1Buffer, Worker2Buffer) ->
    receive
        CtxBaggage ->
            ProbeCtx = maps:get(<<"probe_ctx">>, CtxBaggage),
            WorkerCtx = maps:get(<<"worker_ctx">>, CtxBaggage),
            WorkerPid = maps:get(<<"worker_id">>, CtxBaggage),
            ProbePid = maps:get(<<"probe_id">>, CtxBaggage),
            proc_lib:spawn_link(fun() ->
                        otel_ctx:attach(ProbeCtx),
                        ?set_current_span(WorkerCtx),
                        Loops = rand:uniform(Y),
                        loop(Loops),

                       Prob = rand:uniform(),
                         if Prob > 0.5 -> 
                            timer:sleep(10);
                        true ->
                            ok
                        end,
                        otel_wrapper:end_span(WorkerCtx, WorkerPid),
                        ?set_current_span(ProbeCtx),
                        otel_wrapper:end_span(ProbeCtx, ProbePid)
                    end),
           
                Worker2Buffer ! done,   
            worker_loop(worker_2, Y,  Worker1Buffer, Worker2Buffer)
    end.

loop(0) -> ok;
loop(N) -> loop(N - 1).

stop(_State) ->
    ok.
