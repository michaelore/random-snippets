-module(gameserver).
-behaviour(gen_server).
-export([start/0, introduce/0, send_update/1, stop/0, request_matrix/0]).
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

init(Mat) ->
    yaws:start_embedded("./www/"),
    sender:start(sender),
    {ok, Mat}.

handle_call(matrix_please, _, Mat) ->
    {reply, Mat}.

handle_cast(hello, Mat) ->
    sender:add(sender, From),
    {noreply, Mat, Mat};
handle_cast({update, Update}, Mat) ->
    sender:send(sender, {update, Update}),
    NewMat = update(Update, Mat),
    {noreply, NewMat};
handle_cast(stop, Mat) ->
    {stop, "'stop' was cast", Mat}.

update({Id, Image}, Mat) ->
    {X, Y} = parse_id(Id),
    array:set((X-1)+10*(Y-1), New, Mat).

parse_id([_,X,_,Y|_]) ->
    {X-48, Y-48}.

terminate(R, _) ->
    sender:send(sender, {terminated, R}),
    sender:stop(sender).
