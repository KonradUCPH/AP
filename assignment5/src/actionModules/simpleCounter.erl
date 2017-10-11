-module(simpleCounter).
-export([initialise/1, action/3]).

initialise(_Arg) -> {ok, 0}.

action(_Req, _Server, State) ->
    NewState = State + 1,
    Content = io_lib:print(NewState),
    {new_state, Content, NewState}.