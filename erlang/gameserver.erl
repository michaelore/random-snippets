-module(gameserver).
-behaviour(gen_server).
-export([start/0, introduce/0, send_update/1, stop/0, request_matrix/0, store/2, lookup/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

InitMat = array:map(fun(_) -> "/www/images/x.png" end, array:new(100)),

start() ->
    start_link({local, gameserver}, gameserver, InitMat, []).

introduce() ->
    gen_server:cast(gameserver, hello).

request_matrix() ->
    gen_server:call(gameserver, matrix_please).

send_update(Update) ->
    gen_server:cast(gameserver, {update, Update}).

stop() ->
    gen_server:cast(gameserver, stop).

store(Key, Value) ->
    gen_server:cast(gameserver, {store, Key, Value}).

lookup(Key) ->
    gen_server:call(gameserver, {lookup, Key}).

init(Mat) ->
    yaws:start_embedded("./www/"),
    sender:start(sender),
    {ok, Mat}.

handle_call(matrix_please, _, {Mat, Dict}) ->
    {reply, Mat, {Mat, Dict}};
handle_call({lookup, Key}, _, {Mat, Dict}) ->
    {reply, fetch(Key, Dict), {Mat, Dict}}.

handle_cast(hello, {Mat, Dict}) ->
    sender:add(sender, From),
    {noreply, Mat, {Mat, Dict}};
handle_cast({update, Update}, {Mat, Dict}) ->
    sender:send(sender, {update, Update}),
    NewMat = update(Update, {Mat, Dict}),
    {noreply, {NewMat, Dict}};
handle_cast({store, Key, Value}, {Mat, Dict}) ->
    NewDict = dict:store(Key, Value, Dict),
    {noreply, {Mat, NewDict}};
handle_cast(stop, {Mat, Dict}) ->
    {stop, "'stop' was cast", {Mat, Dict}}.

update({Id, Image}, Mat) ->
    {X, Y} = parse_id(Id),
    array:set(X+10*Y, New, Mat).

parse_id([_,X,_,Y|_]) ->
    {X-48, Y-48}.

terminate(R, _) ->
    sender:send(sender, {terminated, R}),
    sender:stop(sender).
