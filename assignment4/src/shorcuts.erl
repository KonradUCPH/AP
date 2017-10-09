make:all([load]).

f().
{ok,S} = kaboose:start().
{ok,R} = kaboose:get_a_room(S).
kaboose:add_question(R, {"What Is your age?", [{correct, "24"}, "25"]}).
kaboose:add_question(R, {"What Is your name?", [{correct, "Konrad"}, "Steffan"]}).
{Aroom, CRef} = kaboose:play(R).
{ok, Q} = kaboose:next(Aroom).
{ok, Ref1} = kaboose:join(Aroom, "konrad").
{ok, Ref2} = kaboose:join(Aroom, "Test").
kaboose:leave(Aroom, Ref2).




receive M -> M after 500 -> timeout end.