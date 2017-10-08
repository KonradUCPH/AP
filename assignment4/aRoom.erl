-module(aRoom).

-import(basicserver, [request_reply/2, async/2, handle/2]).
-export([start/1, init/0]).


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

next({aRoom, AroomPid}) ->
    request_reply(AroomPid, {nextQuestion, self()}).


init() -> #{}.

handle({nextQuestion, CallerID}, State) ->
    if CallerID /= maps:get(cRef, State) ->
        {error, who_are_you}
    end