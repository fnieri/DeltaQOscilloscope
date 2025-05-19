
-module(otel_wrapper_benchmark).
-export([run/0, run_test/1]).

-define(NUM_REQUESTS, 25000).
-define(SPAN_NAME, <<"bench_span">>).
-define(OUTDIR, "benchmark_results").

run() ->
    filelib:ensure_dir(?OUTDIR ++ "/dummy.txt"),
    otel_wrapper:set_stub_running(true),
    ets:insert(timeout_registry, {?SPAN_NAME, 10000}),
    
    %run_test(start_span),
    run_test(fail_span),
    run_test(with_span).

run_test(Type) ->
    io:format("Benchmarking ~p/~n", [Type]),
    {Label, Fun} = case Type of
        start_span -> 
            {start_span, fun() -> 
                {Ctx, Pid} = otel_wrapper:start_span(?SPAN_NAME),
                otel_wrapper:end_span(Ctx, Pid)
            end};
        fail_span -> 
            {fail_span, fun() ->
                {_, Pid} = otel_wrapper:start_span(?SPAN_NAME),
                otel_wrapper:fail_span(Pid)
            end};
        with_span ->
            {with_span, fun() ->
                otel_wrapper:with_span(?SPAN_NAME, fun() -> ok end)
            end}
    end,

    Times = sequential_test(Fun, ?NUM_REQUESTS, []),
    print_stats(Label, Times),
    export_csv(Label, Times).


sequential_test(_, 0, Acc) -> lists:reverse(Acc);
sequential_test(Fun, N, Acc) ->
    Remaining = ?NUM_REQUESTS - N,
    maybe_pause(Remaining),
    {TimeMicros, _} = timer:tc(Fun),
    sequential_test(Fun, N - 1, [TimeMicros | Acc]).

maybe_pause(N) when N > 0, N rem 1000 =:= 0 ->
    timer:sleep(500);
maybe_pause(_) ->
    ok.
%% Print basic and advanced stats
print_stats(Name, Times) ->
    Sorted = lists:sort(Times),
    Total = lists:sum(Sorted),
    Min = hd(Sorted),
    Max = lists:last(Sorted),
    Len = length(Sorted),
    Avg = Total / Len,
    Median = lists:nth(Len div 2, Sorted),
    P95 = lists:nth(round(Len * 0.95), Sorted),
    StdDev = stddev(Sorted, Avg),

    io:format("~p Results:~n", [Name]),
    io:format("  Total Time     = ~p µs~n", [Total]),
    io:format("  Avg per Call   = ~.2f µs~n", [Avg]),
    io:format("  Min            = ~p µs~n", [Min]),
    io:format("  Max            = ~p µs~n", [Max]),
    io:format("  Median         = ~p µs~n", [Median]),
    io:format("  95th Percentile= ~p µs~n", [P95]),
    io:format("  Std Deviation  = ~.2f µs~n~n", [StdDev]).

%% Standard deviation
stddev(List, Mean) ->
    SqDiffs = [math:pow(X - Mean, 2) || X <- List],
    math:sqrt(lists:sum(SqDiffs) / length(SqDiffs)).

%% Export all times to CSV
export_csv(Name, Times) ->
    Filename = io_lib:format("~s/~p.csv", [?OUTDIR, Name]),
    {ok, File} = file:open(Filename, [write]),
    file:write(File, <<"Request,Time(µs)\n">>),
    lists:foldl(fun({T, I}, ok) ->
        Line = io_lib:format("~p,~p\n", [I, T]),
        file:write(File, Line)
    end, ok, lists:zip(Times, lists:seq(1, length(Times)))),
    file:close(File),
    io:format("CSV exported to: ~s~n~n", [Filename]).

