-module(faultyCounter).
-export([initialise/1, action/3]).

% is a callback for the actionModuleServer
-behaviour(actionModuleServer).

initialise(_Arg) -> {ok, 0}.

action(_Req, _Server, State) when State =:= 2 ->
    exit(faulty_counter_killing_myself);
action(_Req, _Server, State) ->
    NewState = State + 1,
    Content = io_lib:print(NewState), % to string
    {new_state, Content, NewState}.