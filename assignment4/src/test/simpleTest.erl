-module(simpleTest).
-export([test/0]).

test() ->
    {ok, S} = kaboose:start(),
    {ok, R} = kaboose:get_a_room(S),
    kaboose:add_question(R, {"Who made this file?", ["noone", {correct, "someone"}, "someone else"]}),
    {_ActvRm, _Cref} = kaboose:play(R).