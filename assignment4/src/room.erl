%%% AP assignment 4 
%%% Per Stefen Czolbe, Konrad Gnoinski
%%% Oct 2017
%%% This file contains the process for a room blueprint of the kaboose game

-module(room).

-import(basicServer, [request_reply/2, async/2]).
-import(activeRoom, [start/1]).
-export([start/0, init/0, add_question/2, handle/2,get_questions/1, play/1]).


% start the server
start() -> 
    RoomPid = basicServer:start(room),
    if 
        RoomPid == [] ->
            {error, could_not_spawn_a_process};
        true ->
            {ok, {room, RoomPid}}
    end.

%% client API functions

% Question structure should be {Description, [{correct,Text}, Text]}
add_question({room, RoomPid}, Question) -> 
    async(RoomPid ,{addQuestion, Question}),
    ok.

get_questions({room, RoomPid}) -> 
    request_reply(RoomPid, {getQuestions}).

play({room, RoomPid}) ->
    request_reply(RoomPid, {activateRoom, self()}).


%% internal implementation

init() ->  [].

handle({addQuestion, Question}, State) ->
    State1 = State ++ [Question],
    {ok, State1};
handle({getQuestions}, State) ->
    {State, State};
handle({activateRoom, Conductor}, State) ->
    Reply = activeRoom:start({Conductor, State}),
    {Reply, State}.