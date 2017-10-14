-module(hiphop).
-export([server/0, try_it/2]).

server() ->
    {ok, F} = flamingo:new("The HipHop Server, powered by FlamingoTM"),
    flamingo:route(F, ["/hip"], simpleCounter, none),
    flamingo:route(F, ["/hop"], simpleCounter, none),
    flamingo:route(F, ["/hi"], hello, none),
    flamingo:route(F, ["/f"], faultyCounter, none),
    F.

try_it(Server, Path) ->
    Me = self(),
    Ref = make_ref(),
    flamingo:request(Server, {Path, []}, Me, Ref),
    receive
        {Ref, Reply} -> Reply
    after
        10000 -> timeout
    end.