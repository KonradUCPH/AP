-module(simpleCounter).
-export([initialise/1, action/3]).

% is a callback for the actionModuleServer
-behaviour(actionModuleServer).

initialise(_Arg) -> {ok, 0}.

action(_Req, _Server, State) ->
    NewState = State + 1,
    Content = io_lib:print(NewState), % to string
    {new_state, Content, NewState}.