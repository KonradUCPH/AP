-module(room).

-import(basicserver, [request_reply/2, async/2]).
-import(aRoom, [start/1]).
-export([start/0, init/0, add_question/2, handle/2,get_questions/1, play/1]).


start() -> 
    RoomPid = basicserver:start(room),
    if 
        RoomPid == [] ->
            {error, "Could not spawn a process!"};
        true ->
            {ok, {room, RoomPid}}
    end.

% Question structure should be {Description, [{correct,Text}, Text]}
add_question({room, RoomPid}, Question) -> 
    async(RoomPid ,{addQuestion, Question}),
    ok.

get_questions({room, RoomPid}) -> 
    request_reply(RoomPid, {getQuestions}).

play({room, RoomPid}) ->
    request_reply(RoomPid, {activateARoom, self()}).


init() ->  [].

handle({addQuestion, Question}, State) ->
    State1 = State ++ [Question],
    {ok, State1};
handle({getQuestions}, State) ->
    {State, State};
handle({activateARoom, Conductor}, State) ->
    Reply = aRoom:start({Conductor, State}),
    {Reply, State}.