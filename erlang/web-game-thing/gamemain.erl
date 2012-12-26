-module(gamemain).
-export([main/0]).

main() ->
    code:add_path("/usr/lib/yaws/ebin"),
    gameserver:start().
