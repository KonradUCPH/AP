-module(actionModuleCounter).
-export([initialise/1, action/3]).

% is a callback for the actionModuleServer
-behaviour(actionModuleServer).

initialise(Initial) -> {ok, Initial}.

action({"/inc_with", [{"x", N} | _ ]}, _Server, State) ->
    NewState = State + N,
    Content = io_lib:print(NewState),
    {new_state, Content, NewState};
action({"/dec_with", [{"x", N} | _ ]}, _Server, State) ->
    NewState = State - N,
    Content = io_lib:print(NewState),
    {new_state, Content, NewState}.