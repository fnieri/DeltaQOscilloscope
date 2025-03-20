
-module(sample).
-export([start/3,  send/2, worker_loop/5, worker_buffer/4]).


start(X, Y, K) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8080, [binary, {packet, 0}, {active, false}]),

    % Step 1: Spawn worker buffer processes with undefined worker placeholders
    Worker1Buffer = spawn_opt(fun() -> worker_buffer(worker_1, undefined, K, 0) end, [{scheduler, 3}]),
    Worker2Buffer = spawn_opt(fun() -> worker_buffer(worker_2, undefined, K, 0) end, [{scheduler, 4}]),

    
    % Step 2: Spawn worker loop processes with correct buffer PIDs
    Worker2 = spawn_opt(fun() -> worker_loop(worker_2, Y, Socket, Worker1Buffer, Worker2Buffer) end, [{scheduler, 1}]),
    Worker1 = spawn_opt(fun() -> worker_loop(worker_1, Y, Socket, Worker1Buffer, Worker2Buffer) end, [{scheduler, 2}]),

    % Step 3: Update the worker buffers with the correct worker PIDs
    Worker1Buffer ! {set_worker, Worker1},
    Worker2Buffer ! {set_worker, Worker2},

    % Start sender process
    Sender = spawn(fun() -> send(X, Worker1Buffer) end),

    {Worker1, Worker2, Worker1Buffer, Worker2Buffer, Sender}.


send(X, Worker1Buffer) ->
    Delay = -math:log(rand:uniform()) / X, 
    timer:sleep(trunc(Delay * 1000)),
    StartTimeNano = erlang:system_time(nanosecond),
    Worker1Buffer ! {StartTimeNano},
    send(X, Worker1Buffer).

worker_buffer(worker_1, Worker1, K, QueueLength) ->
    receive
        {set_worker, NewWorker} ->  % Update worker PID when it is available
            worker_buffer(worker_1, NewWorker, K, QueueLength);
        {StartTimeNano} when QueueLength < K ->
            Worker1 ! {StartTimeNano},
            worker_buffer(worker_1, Worker1, K, QueueLength + 1);
        {processed} ->
            worker_buffer(worker_1, Worker1, K, QueueLength - 1)
    end;

worker_buffer(worker_2, Worker2, K, QueueLength) ->
    receive
        {set_worker, NewWorker} ->  % Update worker PID when it is available
            worker_buffer(worker_2, NewWorker, K, QueueLength); 
        {StartTimeNano, PrevMessage} when QueueLength < K ->
            Worker2 ! {StartTimeNano, PrevMessage},
            worker_buffer(worker_2, Worker2, K, QueueLength + 1);
        {processed} ->
            worker_buffer(worker_2, Worker2, K, QueueLength - 1)
    end.

worker_loop(worker_1, Y, Socket, Worker1Buffer, Worker2Buffer) ->
    receive
        {StartTimeNano} ->
            
            StartNano = erlang:system_time(nanosecond), 
            Loops = rand:uniform(Y),
            loop(Loops),
            EndNano = erlang:system_time(nanosecond),

            Message = io_lib:format("worker_1: Start=~p End=~p~n", [StartNano, EndNano]),  
            
            Worker2Buffer ! {StartTimeNano, Message},
            Worker1Buffer ! {processed},
            worker_loop(worker_1, Y, Socket, Worker1Buffer, Worker2Buffer)
    end;

worker_loop(worker_2, Y, Socket, Worker1Buffer, Worker2Buffer) ->
    receive
        {StartTimeNano, PrevMessage} ->  
            
            StartNano = erlang:system_time(nanosecond),
            Loops = rand:uniform(Y),
            loop(Loops),
            EndNano = erlang:system_time(nanosecond),
            Worker2Buffer ! {processed},
            % Construct messages
            Worker2Message = io_lib:format("worker_2: Start=~p End=~p~n", [StartNano, EndNano]),
            FullLoopMessage = io_lib:format("full_loop: Start=~p End=~p~n", [StartTimeNano, EndNano]),

            % Send messages in correct order
            gen_tcp:send(Socket, PrevMessage), 
            gen_tcp:send(Socket, Worker2Message),
            gen_tcp:send(Socket, FullLoopMessage),
            
            worker_loop(worker_2, Y, Socket, Worker1Buffer, Worker2Buffer)
    end.
loop(0) -> ok;
loop(N) -> loop(N - 1).
