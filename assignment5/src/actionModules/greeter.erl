-module(greeter).
-export([initialise/1, action/3]).

% is a callback for the actionModuleServer
-behaviour(actionModuleServer).

initialise(_Arg) -> {ok, nothing}.

action({_Path, [{"name", Name} | _ ]}, Server, _) ->
    {no_change,
     lists:concat(["Greetings ", Name, "\n",
                   "You have reached ", Server])}.