% The routing module, registers and looks up routes
% this doubles as an actionModule, to rely on much of the general
% robustness architecture

-module(router).
-export([initialise/1, action/3]).

% is a callback for the actionModuleServer
-behaviour(actionModuleServer).

initialise(_Args) -> {ok, #{}}.

%data structure: map of prefix to Action module reference (Pid)

% adds a routing group to the router
action({add, Prefixes, ActionPid}, _Server, State) ->
    NewState = addAll(Prefixes, ActionPid, State),
    {new_state, {ok, ActionPid}, NewState};

% looks up a request
action({get, Request}, _Server, State) ->
    {Prefix, _Params} = Request,
    PrefixMatches = maps:filter(fun(K, _) -> string:left(K, string:len(Prefix)) =:= Prefix end, State),
    L = maps:to_list(PrefixMatches),
    Matching = lists:sort(fun({K1, _}, {K2, _}) -> string:len(K1) > string:len(K2) end, L),
    case Matching of
        [{_, ActionPid}| _] -> {no_change, {ok, ActionPid}};
        [] -> {no_change, {error, 404}}
    end.


addAll([], _, State) -> State;
addAll([Prefix|Prefixes], ActionPid, State) ->
    State1 = State#{Prefix => ActionPid},
    addAll(Prefixes, ActionPid, State1).