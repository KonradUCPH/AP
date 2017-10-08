-module(kb).

-import(basicserver, [request_reply/2]).
-export([start/0, init/0, get_a_room/1, handle/2]).



start() -> 
    ServerPid = basicserver:start(kb),
    if 
        ServerPid == [] ->
            {error, "Could not spawn a process!"};
        true ->
            {ok, {server, ServerPid}}
    end.

get_a_room({server, Pid}) -> request_reply(Pid, {createRoom}).



init() -> nothing.

handle({createRoom}, State) ->
    Reply = room:start(),
    {Reply, State}.
