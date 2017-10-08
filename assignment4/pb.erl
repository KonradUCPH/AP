%% Basic concurrent phone-book server callback module
%%
%% Author: Ken Friis Larsen <kflarsen@diku.dk>

-module(pb).
-compile([export_all]).
-import(basicserver, [request_reply/2]).



%% Interface
% We assume that the service will be started under the name phonebook
start()         -> basicserver:start(phonebook, pb).
add(Contact)    -> request_reply(phonebook, {add, Contact}).
list_all()      -> request_reply(phonebook, list_all).
update(Contact) -> request_reply(phonebook, {update, Contact}).








%% Callback functions
init() -> #{}.

handle({add, {Name, _, _} = Contact}, Contacts) ->
    case maps:is_key(Name, Contacts) of
        false -> {ok, Contacts#{Name => Contact}};
        true  -> {{error, Name, is_already_there},
                  Contacts}
    end;
handle(list_all, Contacts) ->
    List = maps:to_list(Contacts),
    {{ok, lists:map(fun({_, C}) -> C end, List)},
     Contacts};
handle({update, {Name, _, _} = Contact}, Contacts) ->
    {ok, Contacts#{Name => Contact}}.
