-module(hello).
-export([initialise/1, action/3]).

initialise(_Arg) -> {ok, nothing}.

action(_Req, _Server, _State) ->
    {no_change, "Wassup"}.