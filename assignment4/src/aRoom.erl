%%% AP assignment 4 
%%% Per Stefen Czolbe, Konrad Gnoinski
%%% Oct 2017
%%% This file contains the process for an active game room of the kaboose game

-module(aRoom).

-import(basicserver, [request_reply/2, async/2]).
-export([start/1, init/0, next/1, handle/2]).


% start the server
start({Conductor, Questions}) -> 
    State = #{questions => Questions, 
                cRef => Conductor, 
                players => [], 
                questionActive => false},
    ARoomPid = basicserver:start(aRoom, State),
    if 
        ARoomPid == [] ->
            {error, could_not_spawn_a_process};
        true ->
            {{activeRoom, ARoomPid}, Conductor}
    end.

%% client API functions

next({aRoom, AroomPid}) ->
    request_reply(AroomPid, {nextQuestion, self()}).


%% internal implementation

init() -> #{}.

handle({nextQuestion, Caller}, State) ->
    Conductor = maps:get(cRef, State),
    if Caller /= Conductor ->
        {error, who_are_you}
    end.