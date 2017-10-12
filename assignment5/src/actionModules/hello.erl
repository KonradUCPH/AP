-module(hello).
-export([initialise/1, action/3]).

% is a callback for the actionModuleServer
-behaviour(actionModuleServer).


initialise(_Arg) -> {ok, nothing}.

action(_Req, _Server, _State) ->
    {no_change, "Wassup"}.