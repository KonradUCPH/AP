-module(hiphop).
-export([server/0]).

server() ->
    {ok, F} = flamingo:new("The HipHop Server, powered by FlamingoTM"),
    flamingo:route(F, ["/hip"], simpleCounter, none),
    flamingo:route(F, ["/hop"], simpleCounter, none),
    flamingo:route(F, ["/hi"], hello, none),
    F.