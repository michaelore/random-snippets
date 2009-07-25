-module(gameclient).
-export([start/0].

start() ->
    spawn(fun init/0).

init() ->
    gameserver:introduce(),
    loop(queue:new()).

loop(Updates) ->
    receive
        {update, Update} ->
            loop(queue:in(Update, Updates))
    end.
