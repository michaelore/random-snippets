%Inspiered by the follow article:
%www.trapexit.org/Erlang_and_Neural_Networks
%I ended up writing it from scratch because I didn't understand their implementation.

-module(ann).
-export([perceptron/7, spawnNode/0, spawnNode/1, connectAll/2, spawnInput/1, spawnInput/0, inputPercep/2, extractPIDS/1]).
-define(L, -0.2).

equalS(S1, S2) -> sets:is_subset(S1, S2) and sets:is_subset(S2, S1).

sigmoid(N) ->
    1 / (1 + math:exp(-N)).

ddxSigmoid(N) ->
    math:exp(-N) / (1 + 2*math:exp(-N) + math:exp(-2*N)).

mapS(Fun, Set) ->
    Acc_Fun = fun(E, Acc) -> sets:add_element(Fun(E), Acc) end,
    sets:fold(Acc_Fun, sets:new(), Set).

findPred(Pred, Set) ->
    sets:fold(fun(E, Acc) -> Bool = Pred(E), if Bool -> E; true -> Acc end end, false, Set).

delPred(Pred, Set) ->
    sets:fold(fun(E, Acc) -> Bool = Pred(E), if Bool -> Acc; true -> sets:add_element(E, Acc) end end, sets:new(), Set).

sumS(Set) ->
    sets:fold(fun(E, Acc) -> E + Acc end, 0, Set).

extractPIDS(Set) ->
    mapS(fun({PID, _}) -> PID end, Set).

perceptron(Weights, Inputs, Outputs, Received, ReceivedS, Bias, Net) ->
    RPIDS = extractPIDS(Received),
    RSPIDS = extractPIDS(ReceivedS),
    G1 = equalS(RPIDS, Inputs),
    G2 = equalS(RSPIDS, Outputs),
    Empty = sets:new(),
    if
	G1, RPIDS =/= Empty ->
	    CalcPID = fun(PID) ->
		    {_, Weight} = findPred(fun({P, _}) -> P == PID end, Weights),
		    {_, N} = findPred(fun({P, _}) -> P == PID end, Received),
		    Weight * N end,
	    NewN = sumS(mapS(CalcPID, Inputs)) + Bias,
	    sendOutput(Outputs, sigmoid(NewN)),
	    perceptron(Weights, Inputs, Outputs, Empty, ReceivedS, Bias, NewN);
	G2, RSPIDS =/= Empty ->
	    backProp(Weights, Inputs, Outputs, Received, ReceivedS, Bias, Net);
	true ->
	    receive
		{stimulate, PID, N} ->
		    NewR = sets:add_element({PID, N}, Received),
		    perceptron(Weights, Inputs, Outputs, NewR, ReceivedS, Bias, Net);
		{learn, PID, N} ->
		    NewRS = sets:add_element({PID, N}, ReceivedS),
		    perceptron(Weights, Inputs, Outputs, Received, NewRS, Bias, Net);
		{connect, output, PIDS} ->
		    NewO = lists:foldl(fun(E, Acc) -> sets:add_element(E, Acc) end, Outputs, PIDS),
		    lists:foreach(fun(PID) -> erlang:monitor(process, PID), PID ! {connect, input, [self()]} end, PIDS),
		    perceptron(Weights, Inputs, NewO, Received, ReceivedS, Bias, Net);
		{connect, input, PIDS} ->
		    NewI = lists:foldl(fun(E, Acc) -> sets:add_element(E, Acc) end, Inputs, PIDS),
		    NewW = lists:foldl(fun(E, Acc) ->sets:add_element({E, 0.5}, Acc) end, Weights, PIDS),
		    lists:foreach(fun(PID) -> erlang:monitor(process, PID) end, PIDS),
		    perceptron(NewW, NewI, Outputs, Received, ReceivedS, Bias, Net);
		{'DOWN', _, process, PID, _} ->
		    NewO = sets:del_element(PID, Outputs),
		    NewI = sets:del_element(PID, inputs),
		    NewW = delPred(fun({E, _}) -> E == PID end, Weights),
		    NewR = delPred(fun({E, _}) -> E == PID end, Received),
		    NewRS = delPred(fun({E, _}) -> E == PID end, ReceivedS),
		    perceptron(NewW, NewI, NewO, NewR, NewRS, Bias, Net)
	    end
    end.

sendOutput(Outputs, N) ->
    Fire = fun(PID) -> PID ! {stimulate, self(), N} end,
    mapS(Fire, Outputs).

backProp(Weights, Inputs, Outputs, Received, ReceivedS, Bias, Net) ->
    Sensitivity = sumS(mapS(fun({_, N}) -> N end, ReceivedS)),
    Send = fun(PID) ->
	    {_, Weight} = findPred(fun({P, _}) -> P == PID end, Weights),
	    PID ! {learn, self(), Weight * Sensitivity} end,
    mapS(Send, Inputs),
    NewW = mapS(fun({PID, N}) -> {PID, N + ?L*Sensitivity*ddxSigmoid(Net)*sigmoid(Net)} end, Weights),
    NewB = Bias + ?L*Sensitivity*ddxSigmoid(Net)*sigmoid(Net),
    perceptron(NewW, Inputs, Outputs, Received, sets:new(), NewB, Net).

spawnNode() -> E = sets:new(), spawn(ann, perceptron, [E, E, E, E, E, 1, 0]).
spawnNode(0) -> [];
spawnNode(N) -> [spawnNode()|spawnNode(N-1)].

connectAll(AS, BS) ->
    lists:foreach(fun(A) -> A ! {connect, output, BS} end, AS).

spawnInput() -> E = sets:new(), spawn(ann, inputPercep, [self(), E]).
spawnInput(0) -> [];
spawnInput(N) -> [spawnInput()|spawnInput(N-1)].

inputPercep(Creator, Outputs) ->
    receive
	{pass, N} ->
	    sendOutput(Outputs, N),
	    inputPercep(Creator, Outputs);
	{connect, output, PIDS} ->
	    NewO = lists:foldl(fun(E, Acc) -> sets:add_element(E, Acc) end, Outputs, PIDS),
	    lists:foreach(fun(PID) -> erlang:monitor(process, PID), PID ! {connect, input, [self()]} end, PIDS),
	    inputPercep(Creator, NewO);
	{learn, PID, N} ->
	    Creator ! {report, PID, N},
	    inputPercep(Creator, Outputs)
    end.
