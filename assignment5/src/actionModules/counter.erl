-module(simpleCounter).
-export([initialise/1, action/3]).

initialise(Initial) -> {ok, Initial}.

action({"/inc_with", [{"x", N} | _ ]}, _Server, State) ->
    NewState = State + N,
    Content = io_lib:print(NewState),
    {new_state, Content, NewState};
action({"/dec_with", [{"x", N} | _ ]}, _Server, State) ->
    NewState = State - N,
    Content = io_lib:print(NewState),
    {new_state, Content, NewState}.