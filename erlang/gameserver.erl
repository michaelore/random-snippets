-module(gameserver).
-behaviour(gen_server).
-export([start/0, introduce/0, send_update/1, stop/0, request_matrix/0, store/2, lookup/1]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).
-define(INITMAT, array:from_list(lists:duplicate(100, "/images/x.png"))).

start() ->
    gen_server:start_link({local, gameserver}, gameserver, ?INITMAT, []).

introduce() ->
    gen_server:call({local, gameserver}, hello).

request_matrix() ->
    gen_server:call({local, gameserver}, matrix_please).

send_update(Update) ->
    gen_server:cast({local, gameserver}, {update, Update}).

stop() ->
    gen_server:cast({local, gameserver}, stop).

store(Key, Value) ->
    gen_server:cast({local, gameserver}, {store, Key, Value}).

lookup(Key) ->
    gen_server:call({local, gameserver}, {lookup, Key}).

init(Mat) ->
    {ok, WD} = file:get_cwd(),
    yaws:start_embedded(WD ++ "/www/"),
    sender:start(sender),
    {ok, Mat}.

handle_call(matrix_please, _, {Mat, Dict}) ->
    {reply, array:to_list(Mat), {Mat, Dict}};
handle_call({lookup, Key}, _, {Mat, Dict}) ->
    {reply, dict:fetch(Key, Dict), {Mat, Dict}};
handle_call(hello, From, {Mat, Dict}) ->
    sender:add(sender, From),
    {reply, ok, {Mat, Dict}}.

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
    array:set(X+10*Y, Image, Mat).

parse_id([_,X,_,Y|_]) ->
    {X-48, Y-48}.

terminate(R, _) ->
    sender:send(sender, {terminated, R}),
    sender:stop(sender).
