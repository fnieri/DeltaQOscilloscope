
-module(sample).
-export([start/2, worker_loop/3, send/3]).

start(X, Y) ->
    {ok, Socket} = gen_tcp:connect("127.0.0.1", 8080, [binary, {packet, 0}, {active, false}]),
     Worker1 = spawn_opt(fun() -> worker_loop(worker_1, Y, Socket) end, [{scheduler, 1}]),
    Worker2 = spawn_opt(fun() -> worker_loop(worker_2, Y, Socket) end, [{scheduler, 2}]),
        Sender = spawn(fun() -> send(X, Worker1, Worker2) end),
    {Worker1, Worker2, Sender}.

send(X, Worker1, Worker2) ->
    Delay = -math:log(rand:uniform()) / X, 
    timer:sleep(trunc(Delay * 1000)),
    StartTimeNano = erlang:system_time(nanosecond), 
    Worker1 ! {Worker2, StartTimeNano},
    send(X, Worker1, Worker2).

worker_loop(worker_1, Y, Socket) ->
    receive
        {Worker2, StartTimeNano} ->
            Loops = rand:uniform(Y),
            StartNano = erlang:system_time(nanosecond), 
            loop(Loops),
            EndNano = erlang:system_time(nanosecond),

            Message = io_lib:format("worker_1: Start=~p End=~p~n", [StartNano, EndNano]),  
            
            Worker2 ! {StartTimeNano, Loops, Message}, 
            worker_loop(worker_1, Y, Socket)
    end;

worker_loop(worker_2, Y, Socket) ->
    receive
        {StartTimeNano, Loops, PrevMessage} ->  
            StartNano = erlang:system_time(nanosecond),
            loop(Loops),
            EndNano = erlang:system_time(nanosecond),

            % Construct messages
            Worker2Message = io_lib:format("worker_2: Start=~p End=~p~n", [StartNano, EndNano]),
            FullLoopMessage = io_lib:format("full_loop: Start=~p End=~p~n", [StartTimeNano, EndNano]),

            % Send messages in correct order
            gen_tcp:send(Socket, PrevMessage), 
            gen_tcp:send(Socket, Worker2Message),
            gen_tcp:send(Socket, FullLoopMessage),
            
            worker_loop(worker_2, Y, Socket)
    end.
loop(0) -> ok;
loop(N) -> loop(N - 1).

