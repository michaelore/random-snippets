-module(sender).
-export(start/1, add/2, send/2, stop/1).

start(Name) ->
    put(Name, spawn_link(fun sender/0)).

add(Name, PID) ->
    get(Name) ! {add, PID}.

send(Name, Data) ->
    get(Name) ! {send, Data}.

stop(Name) ->
    unlink(get(Name)),
    get(Name) ! stop.

sender() ->
    sender(sets:new()).
sender(PIDS) ->
    receive
        {add, PID} ->
            monitor(process, PID),
            sender(sets:add_element(PIDS));
        {send, Data} ->
            sets:fold(fun(PID, _) -> gameclient:update(PID, data) end, [], PIDS);
        stop ->
            ok;
        {'DOWN', _, PID, _} ->
            sender(sets:del_element(PID, PIDS))
    end.
