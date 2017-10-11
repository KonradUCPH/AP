%%% AP assignment 4 
%%% Per Stefen Czolbe, Konrad Gnoinski
%%% Oct 2017
%%% This file contains the process for an active game room of the kaboose game

-module(activeRoom).

-import(basicServer, [request_reply/2, async/2]).
-export([start/1, init/0, next/1, handle/2, join/2, leave/2, rejoin/2, timesup/1, debug/1, guess/3, updatepoints/1, distributionListAdd/2]).


% start the server
start({Conductor, Questions}) -> 
    State = #{questions => Questions, % init State dict
                cRef => Conductor, 
                players => #{}, 
                questionActive => false,
                currentPoints => 0,
                activeQ_distribOfOptions => [],
                activeQ_points => #{}
             },
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

timesup({activeRoom, AroomPid}) ->
    request_reply(AroomPid, {timesUp, self()}).

guess({activeRoom, AroomPid}, Ref, Index) -> 
    async(AroomPid, {guess, Ref, Index}).

debug({activeRoom, AroomPid}) ->
    request_reply(AroomPid, {debug}).

updatepoints(AroomPid) ->
    async(AroomPid, {updatePoints}).

%% internal implementation

init() -> #{}.

messageConductor(State, Message) ->
    CRef = maps:get(cRef, State),
    CRef ! {CRef, Message}.

% initialises the list of distributions
distributionListInit(OptionsCnt) ->
    if
        OptionsCnt == 0 -> [];
        true -> [0| distributionListInit(OptionsCnt - 1)]
    end.
% adds an answer to the distribution list
% Answer index is 1-indexed!
distributionListAdd([], _) -> [];
distributionListAdd([H|T], 1) -> [H + 1 | T];
distributionListAdd([H|T], Index) -> [H| distributionListAdd(T, Index - 1)].



% initializes the state with the next question
initQuestion(State) ->
    Questions = maps:get(questions, State),
    [Question|_] = Questions,
    {_, Options} = Question,
    OptionsCnt = length(Options),
    OptionsDist = distributionListInit(OptionsCnt),
    timer:apply_after(500, activeRoom, updatepoints,[self()]),
    State#{ questionActive := true, 
            currentPoints := 1000,
            activeQ_distribOfOptions := OptionsDist,
            activeQ_points := #{}
            }.

% adds points of the current question to the player dictionary
% returns updated player Dict
addPoints([], PlayerDict) -> PlayerDict;
addPoints(CurrentQuestionPointsList, PlayerDict) ->
    [{Nick, NewPoints} | Tail] = CurrentQuestionPointsList,
    PlayerDict1 = addPoints(Tail, PlayerDict),
    Reference = {playerRef, Nick},
    {_, OldPoints, ActiveState} = maps:get(Reference, PlayerDict1), 
    PlayerDict1#{{playerRef, Nick} => {Nick, OldPoints + NewPoints, ActiveState}}.

% transforms the player dict into the format required for total
% player dict format: #{Ref => {Nick, Points, ActiveState}}
% Total format: #{Nick => Points}
getTotal(Players) ->
    getTotal1(maps:to_list(Players)).
getTotal1([]) -> #{};
getTotal1([{_, {Nick, Points, _}}| Tail]) ->
    Dict = getTotal1(Tail),
    Dict#{Nick => Points}.

% ends the current question, 
% adds points to players,
% returns {{ok, Dist, LastQ, Total, Final}, NewState}
endQuestion(State) ->
    % remove question from list
    Questions = maps:get(questions, State),
    [_|NextQuestions] = Questions,
    % gather stats
    OptionsDist = maps:get(activeQ_distribOfOptions, State),
    LastQ = maps:get(activeQ_points, State),
    % add ponts to players
    Players = maps:get(players, State),
    LastQList = maps:to_list(LastQ),
    Players1 = addPoints(LastQList, Players),
    Total = getTotal(Players1),
    State1 = State#{questions := NextQuestions,
                    players := Players1,
                    questionActive := false
                    },
    if
        NextQuestions == [] -> 
            Final = true,
            {{ok, OptionsDist, LastQ, Total, Final}, State1};
        true ->
            Final = false,
            basicServer:kill(),
            {{ok, OptionsDist, LastQ, Total, Final}, State1}
    end.



handle({nextQuestion, Caller}, State) ->
    Conductor = maps:get(cRef, State),
    ActiveQ = maps:get(questionActive, State),
    if 
        Caller /= Conductor ->
            {{error, who_are_you}, State};
        ActiveQ ->
            {{error, has_active_question}, State};
        true -> 
            State1 = initQuestion(State),
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
            {{error, Nickname, is_taken}, State};
        true -> 
            Players1 = Players#{Reference => {Nickname, 0, active}},
            State1 = State#{players := Players1},
            PlayersList = maps:values(Players1),
            NoOfPlayers = activePlayers(PlayersList),
            messageConductor(State1, {player_joined, Nickname, NoOfPlayers}),
            {{ok, Reference}, State1}
    end;

handle({leave, Ref}, State) ->
    Players = maps:get(players, State),
    Player = maps:get(Ref, Players),
    {Nickname, Points, _} = Player,
    Player1 = {Nickname, Points, inactive},
    Players1 = Players#{Ref := Player1},
    State1 = State#{players := Players1},
    PlayersList = maps:values(Players1),
    NoOfPlayers = activePlayers(PlayersList),
    messageConductor(State1, {player_left, Nickname, NoOfPlayers}),
    {ok, State1};

handle({rejoin, Ref}, State) ->
    Players = maps:get(players, State),
    Player = maps:get(Ref, Players),
    {Nickname, Points, _} = Player,
    Player1 = {Nickname, Points, active},
    Players1 = Players#{Ref := Player1},
    State1 = State#{players := Players1},
    PlayersList = maps:values(Players1),
    NoOfPlayers = activePlayers(PlayersList),
    messageConductor(State1, {player_joined, Nickname, NoOfPlayers}),
    {ok, State1};

handle({timesUp, Caller}, State) ->
    Conductor = maps:get(cRef, State),
    QuestionIsActive = maps:get(questionActive, State),
    if 
        Caller /= Conductor ->
            {{error, nice_try}, State};
        not QuestionIsActive ->
            {{error, no_question_asked}, State};
        true ->
            endQuestion(State)
    end;

handle({guess, Ref, Index}, State) ->
    Questions = maps:get(questions, State),
    [Question|_] = Questions,
    {_, Options} = Question, 
    NoOfOptions = length(Options),
    % Check if index is not higher than number of possible answers
    if
        (Index > NoOfOptions) or (Index < 1) ->
            {error, State};
        true -> 
            [Question|_] = Questions,
            {_, Options} = Question,
            Players = maps:get(players, State),
            Player = maps:get(Ref, Players),
            {Nickname, _, _} = Player,
            Apoints = maps:get(activeQ_points, State),
            Iscorrect = isCorrect(Options, Index),
            CurrentPoints = maps:get(currentPoints, State),
            Distribution = maps:get(activeQ_distribOfOptions, State),
            % Getting a points for a current player
            IsAlreadyAnswered = maps:is_key(Nickname, Apoints),
            IsActive = maps:get(questionActive, State),
            if 
                not IsActive ->
                    {error, State};
                IsAlreadyAnswered -> 
                    {error, State};
                Iscorrect -> 
            % Adding points for the user
                    Apoints1 = Apoints#{Nickname => CurrentPoints},
             % keeping the record for a distribution of answers
                    Distribution1 = distributionListAdd(Distribution, Index),
                    State1 = State#{activeQ_points := Apoints1, 
                                    activeQ_distribOfOptions := Distribution1},
                    {ok, State1};
                not Iscorrect ->
            % Adding 0 points for a user to mark users that have already answered
                    Apoints1 = Apoints#{Nickname => 0},
                    Distribution1 = distributionListAdd(Distribution, Index),
                    State1 = State#{activeQ_points := Apoints1, 
                                    activeQ_distribOfOptions := Distribution1},
                    {ok, State1}
            end
        end;

handle({updatePoints}, State) ->
    State1 = State#{currentPoints := 500},
    {ok, State1};


handle({debug}, State) ->
    {State, State}.

isCorrect(Options, Index) ->
    case lists:nth(Index, Options) of
        {correct, _} ->
            true;
        _ ->
            false
    end.

activePlayers([]) -> 0;
activePlayers([H|T]) -> 
    case H of
        {_, _, active}   -> 1 + activePlayers(T);
        {_, _, inactive} -> activePlayers(T)
    end.


