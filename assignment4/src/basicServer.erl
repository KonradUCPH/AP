%% Simple generic server library
%%
%% Author: Ken Friis Larsen <kflarsen@diku.dk>
%% Modified by Per Steffen Czolbe
%% Date: October, 2017

-module(basicServer).
-export([start/1, start/2, kill/0, request_reply/2, async/2]).

% start the server. Calls init() function of given module. 
% this module needs to supply the initial state.
start(Mod) ->
    spawn(fun() -> loop(Mod, Mod:init()) end).
% spawn for non static initial state
start(Mod, InitialState) ->
    spawn(fun() -> loop(Mod, InitialState) end).

request_reply(Pid, Request) ->
    Pid ! {request_reply, self(), Request},
    receive
        {Pid, Reply} -> Reply
    end.

async(Pid, Message) -> 
    Pid ! {async, Message},
    ok.

% allows the process to kill itself
kill() ->
    Me = self(),
    Me ! {kill, Me},
    goodbye.

loop(Mod, State) ->
    Me = self(),
    receive
        {request_reply, From, Request} ->
            {Reply, State1} = Mod:handle(Request, State),
            From ! {self(), Reply},
            loop(Mod, State1);
        {async, Message} ->
            {_, State1} = Mod:handle(Message, State),
            loop(Mod, State1);
        {kill, Me} ->
            dead
    end.
