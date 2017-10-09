%%% AP assignment 4 
%%% Per Stefen Czolbe, Konrad Gnoinski
%%% Oct 2017
%%% This file contains the process for an active game room of the kaboose game

-module(activeRoom).

-import(basicServer, [request_reply/2, async/2]).
-export([start/1, init/0, next/1, handle/2, join/2, leave/2, rejoin/2, debug/1]).


% start the server
start({Conductor, Questions}) -> 
    State = #{questions => Questions, % init State dict
                cRef => Conductor, 
                players => #{}, 
                questionActive => false},
    ARoomPid = basicServer:start(activeRoom, State),
    if 
        ARoomPid == [] ->
            {error, could_not_spawn_a_process};
        true ->
            {{activeRoom, ARoomPid}, Conductor}
    end.

%% client API functions

next({activeRoom, AroomPid}) ->
    request_reply(AroomPid, {nextQuestion, self()}).

join({activeRoom, AroomPid}, Nickname) -> 
    request_reply(AroomPid, {join, Nickname}).

leave({activeRoom, AroomPid}, Ref) -> 
    async(AroomPid, {leave, Ref}).

rejoin({activeRoom, AroomPid}, Ref) -> 
    async(AroomPid, {rejoin, Ref}).

debug({activeRoom, AroomPid}) ->
    request_reply(AroomPid, {debug}).

%% internal implementation

init() -> #{}.

messageConductor(State, Message) ->
    CRef = maps:get(cRef, State),
    async(CRef, Message).

handle({nextQuestion, Caller}, State) ->
    Conductor = maps:get(cRef, State),
    ActiveQ = maps:get(questionActive, State),
    if 
        Caller /= Conductor ->
            {{error, who_are_you}, State};
        ActiveQ ->
            {{error, has_active_question}, State};
        true -> 
            State1 = State#{questionActive := true},
            Questions = maps:get(questions, State),
            [Question1|_] = Questions,
            {{ok, Question1}, State1}
    end;

handle({join, Nickname}, State) ->
    Players = maps:get(players, State),
    Reference = {playerRef, Nickname},
    NickExists = maps:is_key(Reference, Players),
    if 
        NickExists ->
            {{error, is_taken}, State};
        true -> 
            Players1 = Players#{Reference => {Nickname, 0, active}},
            State1 = State#{players := Players1},
            messageConductor(State1, player_has_joined),
            {{ok, Reference}, State1}
    end;

handle({leave, Ref}, State) ->
    Players = maps:get(players, State),
    Player = maps:get(Ref, Players),
    {Nickname, Points, _} = Player,
    Player1 = {Nickname, Points, inactive},
    Players1 = Players#{Ref := Player1},
    State1 = State#{players := Players1},
    messageConductor(State1, player_has_left),
    {ok, State1};

handle({rejoin, Ref}, State) ->
    Players = maps:get(players, State),
    Player = maps:get(Ref, Players),
    {Nickname, Points, _} = Player,
    Player1 = {Nickname, Points, active},
    Players1 = Players#{Ref := Player1},
    State1 = State#{players := Players1},
    messageConductor(State1, player_has_rejoined),
    {ok, State1};

handle({debug}, State) ->
    {State, State}.



