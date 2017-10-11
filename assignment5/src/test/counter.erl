-module(counter).
-export([server/0]).

server() ->
    {ok, F} = flamingo:new("The Counter Server, powered by FlamingoTM"),
    flamingo:route(F, ["/inc_with", "/dec_with"], counter, 0),
    F.