
-module(ftf).
-export([start/2, start/3, send/2, worker_loop/5, stop/1, worker_buffer/4, coordinator/2]).

-include_lib("opentelemetry_api/include/otel_tracer.hrl").

start(_Type, _Args) ->
    {ok, _} = application:ensure_all_started(opentelemetry),
    {ok, Pid} = example_sup:start_link(),
    {ok, Pid}.

start(X, Y, K) ->
    Worker1Buffer = spawn_opt(fun() -> worker_buffer(worker_1, undefined, K, 0) end, [{scheduler, 3}]),
    Worker2Buffer = spawn_opt(fun() -> worker_buffer(worker_2, undefined, K, 0) end, [{scheduler, 4}]),
    Worker1 = spawn_opt(fun() -> worker_loop(worker_1, Y, Worker1Buffer, Worker2Buffer, undefined) end, [{scheduler, 1}]),
    Worker2 = spawn_opt(fun() -> worker_loop(worker_2, Y, Worker1Buffer, Worker2Buffer, undefined) end, [{scheduler, 1}]),

    Worker1Buffer ! {set_worker, Worker1},
    Worker2Buffer ! {set_worker, Worker2},

    register(worker_1, Worker1Buffer),
    register(worker_2, Worker2Buffer),
    spawn(fun() -> send(X, {Worker1Buffer, Worker2Buffer}) end),

    {ok, self()}.

send(X, {Worker1Buffer, Worker2Buffer}) ->
    Delay = -math:log(rand:uniform()) / X,
    timer:sleep(trunc(Delay * 1000)),

    B = maps:new(),
    {ProbeCtx, ProbePid} = dqsd_otel:start_span(<<"ftf">>, #{attributes => [{carmine, <<"sossio">>}]}),
    B1 = maps:put(<<"probe_id">>, ProbePid, B),
    Baggage = maps:put(<<"probe_ctx">>, ProbeCtx, B1),

    % Create coordinator per probe
    CoordinatorPid = spawn(fun() -> coordinator(ProbeCtx, ProbePid) end),
    FullBaggage = maps:put(<<"coordinator">>, CoordinatorPid, Baggage),

    Worker1Buffer ! FullBaggage,
    Worker2Buffer ! FullBaggage,

    send(X, {Worker1Buffer, Worker2Buffer}).

coordinator(ProbeCtx, ProbePid) ->
    receive
        {done, _FromWorker} ->
            dqsd_otel:end_span(ProbeCtx, ProbePid)
    end.

worker_buffer(WorkerName, WorkerPid, K, QueueLength) ->
    receive
        {update_k, NewK} ->
            worker_buffer(WorkerName, WorkerPid, NewK, QueueLength);

        {set_worker, NewWorker} ->
            worker_buffer(WorkerName, NewWorker, K, QueueLength);

        #{<<"probe_ctx">> := _} = CtxBaggage when QueueLength < K ->
            {WorkerCtx, SpanPid} = dqsd_otel:start_span(atom_to_binary(WorkerName)),
            B = maps:put(<<"worker_id">>, SpanPid, CtxBaggage),
            Baggage = maps:put(<<"worker_ctx">>, WorkerCtx, B),
            WorkerPid ! Baggage,
            worker_buffer(WorkerName, WorkerPid, K, QueueLength + 1);

        done ->
            worker_buffer(WorkerName, WorkerPid, K, QueueLength - 1);

        #{<<"probe_ctx">> := _} = CtxBaggage when QueueLength >= K ->
            {WorkerCtx, SpanPid} = dqsd_otel:start_span(atom_to_binary(WorkerName)),
            ProbePid = maps:get(<<"probe_id">>, CtxBaggage, undefined),
            dqsd_otel:fail_span(SpanPid),
            dqsd_otel:fail_span(ProbePid),
            worker_buffer(WorkerName, WorkerPid, K, QueueLength)
    end.

worker_loop(WorkerName, Y, Worker1Buffer, Worker2Buffer, _Placeholder) ->
    receive
        CtxBaggage ->
            ProbeCtx = maps:get(<<"probe_ctx">>, CtxBaggage),
            WorkerCtx = maps:get(<<"worker_ctx">>, CtxBaggage),
            WorkerPid = maps:get(<<"worker_id">>, CtxBaggage),
            CoordinatorPid = maps:get(<<"coordinator">>, CtxBaggage),
            SelfBuffer = case WorkerName of
                            worker_1 -> Worker1Buffer;
                            worker_2 -> Worker2Buffer
                        end,
            proc_lib:spawn_link(fun() ->
                otel_ctx:attach(ProbeCtx),
                ?set_current_span(WorkerCtx),
                case WorkerName of
                    worker_2 -> timer:sleep(20);
                    _ -> ok
                end,
                Loops = rand:uniform(Y),
                loop(Loops),
                dqsd_otel:end_span(WorkerCtx, WorkerPid),
                ?set_current_span(ProbeCtx),
                CoordinatorPid ! {done, WorkerName}
            end),
            SelfBuffer ! done,
            worker_loop(WorkerName, Y, Worker1Buffer, Worker2Buffer, _Placeholder)
    end.

loop(0) -> ok;
loop(N) -> loop(N - 1).

stop(_State) ->
    ok.
