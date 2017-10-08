%%% AP assignment 4 
%%% Per Stefen Czolbe, Konrad Gnoinski
%%% Oct 2017
%%% This file contains the Kaboose Server
%%% compile and load all files with: make:all([load]).

-module(kaboose).
-import(basicServer, [start/1, request_reply/2]).
-export([start/0, 
         init/0, 
         handle/2,
         get_a_room/1,  
         add_question/2, 
         get_questions/1, 
         play/1,
         next/1,
         timesup/1,
         join/2,
         leave/2,
         rejoin/2,
         guess/3
         ]).


% start the server
start() -> 
    ServerPid = basicServer:start(kaboose),
    if 
        ServerPid == [] ->
            {error, could_not_spawn_a_process};
        true ->
            {ok, {server, ServerPid}}
    end.

%% client API functions

get_a_room({server, Pid} = _Server) -> request_reply(Pid, {createRoom}).
add_question(Room, Question) -> room:add_question(Room, Question).
get_questions(Room) -> room:get_questions(Room).
play(Room) -> room:play(Room).
next(ActiveRoom) -> activeRoom:next(ActiveRoom).
timesup(_ActiveRoom) -> undefined.
join(_ActiveRoom, _Nickname) -> undefined.
leave(_ActiveRoom, _Ref) -> undefined.
rejoin(_ActiveRoom, _Ref) -> undefined.
guess(_ActiveRoom, _Ref, _Index) -> undefined.

%% internal implementation

init() -> nothing.

handle({createRoom}, State) ->
    Reply = room:start(),
    {Reply, State}.
