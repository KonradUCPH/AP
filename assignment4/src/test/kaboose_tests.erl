-module(kaboose_tests).
-include_lib("eunit/include/eunit.hrl").
-import(kaboose, [start/0,
         get_a_room/1,  
         add_question/2, 
         get_questions/1, 
         play/1,
         next/1,
         timesup/1,
         join/2,
         leave/2,
         rejoin/2,
         guess/3
         ]).
-export([fakeConductorTimesup/2, fakeConductorNext/2]).

server_start_test() ->
     ?assertMatch({ok, _}, kaboose:start()).

server_start_negative_test() ->
     ?assertNotMatch({error, _}, kaboose:start()).

get_a_room_test() ->
    {ok,S} = kaboose:start(),
    ?assertMatch({ok, _}, kaboose:get_a_room(S)).

get_a_room_negative_test() ->
    {ok,S} = kaboose:start(),
    ?assertNotMatch({error, _}, kaboose:get_a_room(S)).

add_question_negative_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S),
    ?assertNotMatch(ok, kaboose:add_question(R, {"What is your name?", []})).

add_question_error_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S),
    ?assertMatch({error, no_options_given}, kaboose:add_question(R, {"What is your name?", []})).

add_question_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S),
    ?assertMatch(ok, kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]})).

get_questions_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S),
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),
    ?assertMatch([{"What is your name?",[{correct,"Text"},"Text"]}], kaboose:get_questions(R)).

get_questions_order_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S),
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),
    kaboose:add_question(R, {"What is your name2?", [{correct, "Text2"}, "Text2"]}),
    ?assertMatch([{"What is your name?",[{correct,"Text"},"Text"]}, {"What is your name2?", [{correct, "Text2"}, "Text2"]}], kaboose:get_questions(R)).

play_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S),    
    ?assertMatch({_, _}, kaboose:play(R)).

next_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),   
    {Aroom,_} = kaboose:play(R),
    ?assertMatch({ok, {"What is your name?", [{correct, "Text"}, "Text"]}}, kaboose:next(Aroom)).

fakeConductorNext(Aroom, Pid) ->
    Pid ! kaboose:next(Aroom).

next_error_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),   
    {Aroom,_} = kaboose:play(R),
    spawn(kaboose_tests, fakeConductorNext, [Aroom, self()]),
    receive 
        Message -> Message
    end,
    ?assertMatch({error, who_are_you}, Message).

next_has_active_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),  
    kaboose:add_question(R, {"What is your name2?", [{correct, "Text"}, "Text"]}),   
    {Aroom,_} = kaboose:play(R),
    kaboose:next(Aroom),
    ?assertMatch({error, has_active_question}, kaboose:next(Aroom)).

timesup_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    kaboose:next(Aroom),
    ?assertMatch({ok,[_,_],#{},#{},true}, kaboose:timesup(Aroom)). 

timesup_no_question_asked_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    ?assertMatch({error, no_question_asked}, kaboose:timesup(Aroom)).  

fakeConductorTimesup(Aroom, Pid) ->
    Pid ! kaboose:timesup(Aroom).

timesup_non_conductor_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    kaboose:next(Aroom),
    spawn(kaboose_tests, fakeConductorTimesup, [Aroom, self()]),
    receive 
        Message -> Message
    end,
    ?assertMatch({error, nice_try}, Message). 

join_conductor_inform_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    kaboose:join(Aroom, "Konrad"),
    receive 
        Message -> Message
    end,
    Me = self(),
    ?assertMatch({Me,{player_joined, "Konrad", _}}, Message).

join_taken_nick_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    kaboose:join(Aroom, "Konrad"),
    eliminate_msg(),
    ?assertMatch({error, "Konrad", is_taken}, kaboose:join(Aroom, "Konrad")). 

leave_conductor_inform_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    {ok, Ref} = kaboose:join(Aroom, "Konrad"),
    kaboose:leave(Aroom, Ref),
    eliminate_msg(),
    receive 
        Message -> Message
    end,
    Me = self(),
    ?assertMatch({Me,{player_left, "Konrad", _}}, Message).

rejoin_conductor_inform_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    {ok, Ref} = kaboose:join(Aroom, "Konrad"),
    kaboose:leave(Aroom, Ref),
    kaboose:rejoin(Aroom, Ref),
    eliminate_msg(),
    eliminate_msg(),
    receive
        Message -> Message
    end,
    Me = self(),
    ?assertMatch({Me,{player_joined, "Konrad", _}}, Message).

play_1000_points_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    {ok, Ref} = kaboose:join(Aroom, "Konrad"),
    eliminate_msg(),
    kaboose:next(Aroom),
    kaboose:guess(Aroom, Ref, 1),
    {ok,[1,_],Dict,_,true} = kaboose:timesup(Aroom),
    Points = maps:get("Konrad", Dict),
    ?assertMatch(1000 ,Points).

play_500_points_test() ->
    {ok,S} = kaboose:start(),
    {ok,R} = kaboose:get_a_room(S), 
    kaboose:add_question(R, {"What is your name?", [{correct, "Text"}, "Text"]}),    
    {Aroom,_} = kaboose:play(R),
    {ok, Ref} = kaboose:join(Aroom, "Konrad"),
    eliminate_msg(),
    kaboose:next(Aroom),
    timer:sleep(600),
    kaboose:guess(Aroom, Ref, 1),
    {ok,[1,_],Dict,_,true} = kaboose:timesup(Aroom),
    Points = maps:get("Konrad", Dict),
    ?assertMatch(500 ,Points).

eliminate_msg() ->
    receive
        _ -> nom
    end.
    