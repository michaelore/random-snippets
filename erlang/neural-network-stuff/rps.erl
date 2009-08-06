%Neural-network powered rock-paper-scissors program.
%To use open the erlang shell in the installed directory and type:
%c(ann).
%c(rps).
%rps:rps().

-module(rps).
-export([rps/0]).

init([_|[]]) ->
    [];
init([X|XS]) ->
    [X|init(XS)].

shift(X, XS) ->
    [X|init(XS)].
shift(XS) ->
    shift(lists:last(XS), XS).

rps() ->
    Rounds = lists:map(fun(N) -> ann:spawnInput(N) end, [6, 6]),
    Hiddens = ann:spawnNode(10),
    Outputs = ann:spawnNode(3),
    lists:map(fun(Inputs) -> ann:connectAll(Inputs, Hiddens) end, Rounds),
    ann:connectAll(Hiddens, Outputs),
    ann:connectAll(Outputs, [self()]),
    loop(Rounds, lists:duplicate(2, lists:duplicate(6, 0.5)), Outputs, 0, 0, 0).

loop(RoundPIDS, LastRounds, Outputs, Wins, Losses, Draws) ->
    lists:zipwith(fun(PIDS, NS) ->
		lists:zipwith(fun(PID, N) ->
			    PID ! {pass, N} end,
		    PIDS, NS) end,
	RoundPIDS, LastRounds),
    Output = getOutput(sets:new(), Outputs),
    Prediction = getPrediction(lists:map(fun({_, N}) -> N end, Output)),
    Choice = getChoice(),
    CorrectAnswer = shift(Choice),
    Error = lists:zipwith(fun({_, X}, Y) -> X - Y end, Output, CorrectAnswer),
    case Prediction of
	[1, 0, 0] -> io:format("CPU picked Rock\n", []);
	[0, 1, 0] -> io:format("CPU picked Paper\n", []);
	[0, 0, 1] -> io:format("CPU picked Scissors\n", []);
	_ -> io:format("CPU picked Dynamite\n", [])
    end,
    Lose = Prediction == shift(Choice),
    Win = Choice == shift(Prediction),
    Draw = (not Win) and (not Lose),
    if Win -> NewW = Wins + 1, io:format("You win!\n", []);
	true -> NewW = Wins end,
    if Lose -> NewL = Losses + 1, io:format("You lose!\n", []);
	true -> NewL = Losses end,
    if Draw -> NewD = Draws + 1, io:format("Draw!\n", []);
	true -> NewD = Draws end,
    io:format("Wins: ~w, Losses: ~w, Draws: ~w\n", [NewW, NewL, NewD]),
    lists:zipwith(fun(PID, N) -> PID ! {learn, self(), N} end, Outputs, Error),
    NewRounds = shift(lists:append([Choice, Prediction]), LastRounds),
    loop(RoundPIDS, NewRounds, Outputs, NewW, NewL, NewD).

getOutput(Received, Inputs) ->
    RPIDS = ann:extractPIDS(Received),
    SInputs = sets:from_list(Inputs),
    if RPIDS =:= SInputs ->
	    LReceived = sets:to_list(Received),
	    lists:map(fun(PID) -> lists:keyfind(PID, 1, LReceived) end, Inputs);
	true ->
	    receive
		{stimulate, PID, N} ->
		    NewR = sets:add_element({PID, N}, Received),
		    getOutput(NewR, Inputs);
		{report, _, _} ->
		    getOutput(Received, Inputs)
	    end
    end.

getPrediction(Outputs) ->
    Result = lists:map(fun(N) -> random:uniform()*N end, Outputs),
    Max = lists:max(Result),
    lists:map(fun(N) -> if N < Max -> 0; true -> 1 end end, Result).

getChoice() ->
    case io:get_line("1/2/3\nr/p/s\n") of
	"1\n" -> [1, 0, 0];
	"r\n" -> [1, 0, 0];
	"2\n" -> [0, 1, 0];
	"p\n" -> [0, 1, 0];
	"3\n" -> [0, 0, 1];
	"s\n" -> [0, 0, 1];
	_ -> getChoice()
    end.
