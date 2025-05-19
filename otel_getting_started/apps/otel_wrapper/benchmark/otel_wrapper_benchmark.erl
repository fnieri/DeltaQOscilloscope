-module(otel_wrapper_benchmark).
-export([run/0, run/1, benchmark_all/1, benchmark_start_end/1, 
         benchmark_with_span/1, benchmark_fail/1, report_results/1]).

%% Main entry point with default parameters
run() ->
    run(#{
        requests_per_second => 1000,
        duration_seconds => 10,
        concurrent_processes => 4
    }).

%% Main entry point with configurable parameters
run(Config) ->
    % Ensure otel_wrapper is started and stub is enabled
    application:ensure_all_started(otel_wrapper),
    otel_wrapper:set_stub_running(true),
    
    % Set a default timeout for our test spans
    ets:insert(timeout_registry, {<<"benchmark_span">>, 5000}),
    
    % Run all benchmarks
    Results = benchmark_all(Config),
    
    % Report results
    report_results(Results),
    Results.

%% Run all benchmark types
benchmark_all(Config) ->
    io:format("~n=== Starting otel_wrapper Performance Benchmark ===~n"),
    io:format("Configuration: ~p~n", [Config]),
    
    RequestsPerSecond = maps:get(requests_per_second, Config, 1000),
    DurationSeconds = maps:get(duration_seconds, Config, 10),
    ConcurrentProcesses = maps:get(concurrent_processes, Config, 4),
    
    % Run each benchmark type
    StartEndResults = benchmark_start_end(#{
        requests_per_second => RequestsPerSecond,
        duration_seconds => DurationSeconds,
        concurrent_processes => ConcurrentProcesses
    }),
    
    WithSpanResults = benchmark_with_span(#{
        requests_per_second => RequestsPerSecond,
        duration_seconds => DurationSeconds,
        concurrent_processes => ConcurrentProcesses
    }),
    
    FailResults = benchmark_fail(#{
        requests_per_second => RequestsPerSecond,
        duration_seconds => DurationSeconds,
        concurrent_processes => ConcurrentProcesses
    }),
    
    #{
        start_end => StartEndResults,
        with_span => WithSpanResults,
        fail => FailResults
    }.

%% Benchmark start_span and end_span
benchmark_start_end(Config) ->
    io:format("~nBenchmarking start_span/end_span...~n"),
    RequestsPerSecond = maps:get(requests_per_second, Config, 1000),
    DurationSeconds = maps:get(duration_seconds, Config, 10),
    ConcurrentProcesses = maps:get(concurrent_processes, Config, 4),
    
    TotalRequests = RequestsPerSecond * DurationSeconds,
    RequestsPerProcess = TotalRequests div ConcurrentProcesses,
    
    % Start timer
    StartTime = erlang:system_time(microsecond),
    
    % Spawn worker processes
    Pids = [spawn_link(fun() -> 
        start_end_worker(RequestsPerProcess, [])
    end) || _ <- lists:seq(1, ConcurrentProcesses)],
    
    % Collect results
    Results = [receive {done, Measurements} -> Measurements end || _ <- Pids],
    AllMeasurements = lists:flatten(Results),
    
    % End timer
    EndTime = erlang:system_time(microsecond),
    TotalTimeMs = (EndTime - StartTime) / 1000,
    
    % Calculate statistics
    calculate_stats(AllMeasurements, TotalTimeMs, TotalRequests).

%% Worker function for start_span/end_span benchmark
start_end_worker(0, Measurements) ->
    self() ! {done, Measurements};
start_end_worker(Count, Measurements) ->
    StartTime = erlang:system_time(microsecond),
    {Ctx, Pid} = otel_wrapper:start_span(<<"benchmark_span">>),
    MidTime = erlang:system_time(microsecond),
    otel_wrapper:end_span(Ctx, Pid),
    EndTime = erlang:system_time(microsecond),
    
    StartDuration = MidTime - StartTime,
    EndDuration = EndTime - MidTime,
    TotalDuration = EndTime - StartTime,
    
    NewMeasurements = [{StartDuration, EndDuration, TotalDuration} | Measurements],
    start_end_worker(Count - 1, NewMeasurements).

%% Benchmark with_span
benchmark_with_span(Config) ->
    io:format("~nBenchmarking with_span...~n"),
    RequestsPerSecond = maps:get(requests_per_second, Config, 1000),
    DurationSeconds = maps:get(duration_seconds, Config, 10),
    ConcurrentProcesses = maps:get(concurrent_processes, Config, 4),
    
    TotalRequests = RequestsPerSecond * DurationSeconds,
    RequestsPerProcess = TotalRequests div ConcurrentProcesses,
    
    % Start timer
    StartTime = erlang:system_time(microsecond),
    
    % Spawn worker processes
    Pids = [spawn_link(fun() -> 
        with_span_worker(RequestsPerProcess, [])
    end) || _ <- lists:seq(1, ConcurrentProcesses)],
    
    % Collect results
    Results = [receive {done, Measurements} -> Measurements end || _ <- Pids],
    AllMeasurements = lists:flatten(Results),
    
    % End timer
    EndTime = erlang:system_time(microsecond),
    TotalTimeMs = (EndTime - StartTime) / 1000,
    
    % Calculate statistics
    calculate_stats(AllMeasurements, TotalTimeMs, TotalRequests).

%% Worker function for with_span benchmark
with_span_worker(0, Measurements) ->
    self() ! {done, Measurements};
with_span_worker(Count, Measurements) ->
    StartTime = erlang:system_time(microsecond),
    otel_wrapper:with_span(<<"benchmark_span">>, fun() -> ok end),
    EndTime = erlang:system_time(microsecond),
    
    Duration = EndTime - StartTime,
    NewMeasurements = [Duration | Measurements],
    with_span_worker(Count - 1, NewMeasurements).

%% Benchmark fail_span
benchmark_fail(Config) ->
    io:format("~nBenchmarking fail_span...~n"),
    RequestsPerSecond = maps:get(requests_per_second, Config, 1000),
    DurationSeconds = maps:get(duration_seconds, Config, 10),
    ConcurrentProcesses = maps:get(concurrent_processes, Config, 4),
    
    TotalRequests = RequestsPerSecond * DurationSeconds,
    RequestsPerProcess = TotalRequests div ConcurrentProcesses,
    
    % Start timer
    StartTime = erlang:system_time(microsecond),
    
    % Spawn worker processes
    Pids = [spawn_link(fun() -> 
        fail_span_worker(RequestsPerProcess, [])
    end) || _ <- lists:seq(1, ConcurrentProcesses)],
    
    % Collect results
    Results = [receive {done, Measurements} -> Measurements end || _ <- Pids],
    AllMeasurements = lists:flatten(Results),
    
    % End timer
    EndTime = erlang:system_time(microsecond),
    TotalTimeMs = (EndTime - StartTime) / 1000,
    
    % Calculate statistics
    calculate_stats(AllMeasurements, TotalTimeMs, TotalRequests).

%% Worker function for fail_span benchmark
fail_span_worker(0, Measurements) ->
    self() ! {done, Measurements};
fail_span_worker(Count, Measurements) ->
    StartTime = erlang:system_time(microsecond),
    {_, Pid} = otel_wrapper:start_span(<<"benchmark_span">>),
    MidTime = erlang:system_time(microsecond),
    otel_wrapper:fail_span(Pid),
    EndTime = erlang:system_time(microsecond),
    
    StartDuration = MidTime - StartTime,
    FailDuration = EndTime - MidTime,
    TotalDuration = EndTime - StartTime,
    
    NewMeasurements = [{StartDuration, FailDuration, TotalDuration} | Measurements],
    fail_span_worker(Count - 1, NewMeasurements).

%% Calculate statistics from measurements
calculate_stats(Measurements, TotalTimeMs, TotalRequests) ->
    case Measurements of
        [] -> 
            #{};
        [{_, _, _} | _] ->
            % For start_end and fail measurements
            StartTimes = [Start || {Start, _, _} <- Measurements],
            EndTimes = [End || {_, End, _} <- Measurements],
            TotalTimes = [Total || {_, _, Total} <- Measurements],
            
            #{
                avg_start_time_us => lists:sum(StartTimes) / length(StartTimes),
                avg_end_time_us => lists:sum(EndTimes) / length(EndTimes),
                avg_total_time_us => lists:sum(TotalTimes) / length(TotalTimes),
                min_time_us => lists:min(TotalTimes),
                max_time_us => lists:max(TotalTimes),
                throughput => TotalRequests / (TotalTimeMs / 1000),
                total_time_ms => TotalTimeMs,
                total_requests => TotalRequests
            };
        _ ->
            % For with_span measurements
            AvgTime = lists:sum(Measurements) / length(Measurements),
            
            #{
                avg_time_us => AvgTime,
                min_time_us => lists:min(Measurements),
                max_time_us => lists:max(Measurements),
                throughput => TotalRequests / (TotalTimeMs / 1000),
                total_time_ms => TotalTimeMs,
                total_requests => TotalRequests
            }
    end.

%% Report benchmark results
report_results(Results) ->
    io:format("~n=== Benchmark Results ===~n"),
    
    % Report start_span/end_span results
    StartEndResults = maps:get(start_end, Results, #{}),
    io:format("~nstart_span/end_span:~n"),
    io:format("  Average start_span time: ~.2f µs~n", [maps:get(avg_start_time_us, StartEndResults, 0)]),
    io:format("  Average end_span time: ~.2f µs~n", [maps:get(avg_end_time_us, StartEndResults, 0)]),
    io:format("  Average total time: ~.2f µs~n", [maps:get(avg_total_time_us, StartEndResults, 0)]),
    io:format("  Min time: ~.2f µs~n", [maps:get(min_time_us, StartEndResults, 0)]),
    io:format("  Max time: ~.2f µs~n", [maps:get(max_time_us, StartEndResults, 0)]),
    io:format("  Throughput: ~.2f ops/sec~n", [maps:get(throughput, StartEndResults, 0)]),
    
    % Report with_span results
    WithSpanResults = maps:get(with_span, Results, #{}),
    io:format("~nwith_span:~n"),
    io:format("  Average time: ~.2f µs~n", [maps:get(avg_time_us, WithSpanResults, 0)]),
    io:format("  Min time: ~.2f µs~n", [maps:get(min_time_us, WithSpanResults, 0)]),
    io:format("  Max time: ~.2f µs~n", [maps:get(max_time_us, WithSpanResults, 0)]),
    io:format("  Throughput: ~.2f ops/sec~n", [maps:get(throughput, WithSpanResults, 0)]),
    
    % Report fail_span results
    FailResults = maps:get(fail, Results, #{}),
    io:format("~nfail_span:~n"),
    io:format("  Average start_span time: ~.2f µs~n", [maps:get(avg_start_time_us, FailResults, 0)]),
    io:format("  Average fail_span time: ~.2f µs~n", [maps:get(avg_end_time_us, FailResults, 0)]),
    io:format("  Average total time: ~.2f µs~n", [maps:get(avg_total_time_us, FailResults, 0)]),
    io:format("  Min time: ~.2f µs~n", [maps:get(min_time_us, FailResults, 0)]),
    io:format("  Max time: ~.2f µs~n", [maps:get(max_time_us, FailResults, 0)]),
    io:format("  Throughput: ~.2f ops/sec~n", [maps:get(throughput, FailResults, 0)]).
