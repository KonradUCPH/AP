%%% AP assignment 4 
%%% Per Stefen Czolbe, Konrad Gnoinski
%%% Oct 2017
%%% This file contains the Kaboose Server

-module(kb).
-import(basicserver, [request_reply/2]).
-export([start/0, init/0, get_a_room/1, handle/2]).


% start the server
start() -> 
    ServerPid = basicserver:start(kb),
    if 
        ServerPid == [] ->
            {error, could_not_spawn_a_process};
        true ->
            {ok, {server, ServerPid}}
    end.

%% client API functions

get_a_room({server, Pid}) -> request_reply(Pid, {createRoom}).

%% internal implementation

init() -> nothing.

handle({createRoom}, State) ->
    Reply = room:start(),
    {Reply, State}.
