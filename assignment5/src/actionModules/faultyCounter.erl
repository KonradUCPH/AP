-module(faultyCounter).
-export([initialise/1, action/3]).

% is a callback for the actionModuleServer
-behaviour(actionModuleServer).

initialise(_Arg) -> {ok, 0}.

% crashes when counter is 2
action(_Req, _Server, State) when State == 2 ->
    NewState = State + 1,
    Content = io_lib:print(NewState),
    %exit(self(), module_faulty_counter_crashed),
    5 / 0,
    {new_state, Content, NewState};
action(_Req, _Server, State) when State /= 2 ->
    NewState = State + 1,
    Content = io_lib:print(NewState), % to string
    {new_state, Content, NewState}.