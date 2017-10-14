-module(flamingo).
-export([new/1, route/4, request/4]).

%% obeys gen_server behaviour
-behaviour(gen_server).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% define ?SERVER = ?MODULE = modulename
-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

% new(Global) for starting a Flamingo server, with the with the environment Global.
% Returns {ok, Flamingo} on success or {error, Reason} if some error occurred.
new(Global) -> 
    %start router action module
    case actionModuleServer:start(router, none) of
        % start flamingo server
        {ok, RouterPid} -> gen_server:start(?MODULE, 
                                        [actionModuleServer, RouterPid, Global], 
                                        []);  % Module, args, options
        {error, Reason} -> {error, Reason}
    end.

% route(Flamingo, Prefixes, Action, Arg) for registering the action module 
% Action as the action module for the routing group Prefixes at the Flamingo 
% server. The initial local state is computed by calling  
% Action:initialise(Arg), if this fails then the route is not registered. 
% Where Prefixes is a non-empty list of unique strings.
% If the same prefix is registered more than once (perhaps in different routing 
% groups), it is the latest registered action that is matched.
% Returns {ok, Id} on success or {error, Reason} if some error occurred. 
% Where Id is opaque (that is, you choose which type to use), but each Id 
% should be unique.
route(Flamingo, Prefixes, Action, Arg) ->
    gen_server:call(Flamingo, {addRoute, Prefixes, Action, Arg}).

% request(Flamingo, Request, From, Ref) for making the request Request to the 
% Flamingo server.
% This function is non-blocking. When a response is computed, a message of 
% the form {Ref, {Status, Content}} should be sent to From. 
% Remember that a response is a pair consisting of a status code, Status, 
% and a string, Content. If there are no matching routes the status code 
% should be 404, and you decide the content of the string.
request(Flamingo, Request, From, Ref) ->
    gen_server:cast(Flamingo, {get, Request, From, Ref}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([RouterModule, RouterPid, GlobalEnvirmoment]) ->
    State = #{routerPid => RouterPid, 
            routerModule => RouterModule,
            enviroment => GlobalEnvirmoment},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({addRoute, Prefixes, Action, Arg}, _From, State) -> % request_reply
    % start action
    case actionModuleServer:start(Action, Arg) of
        {error, Reason} -> {reply, {error, Reason}, State};
        {ok, ActionPid} ->
            % add routes
            RouterModule = maps:get(routerModule, State),
            RouterPid = maps:get(routerPid, State),
            Reply = RouterModule:action(RouterPid, {add, Prefixes, ActionPid}, env),
            {reply, Reply, State}
    end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast({get, Request, From, Ref}, State) ->  % async
    RouterModule = maps:get(routerModule, State),
    RouterPid = maps:get(routerPid, State),
    Enviroment = maps:get(enviroment, State),
    % get route
    case RouterModule:action(RouterPid, {get, Request}, Enviroment) of
        {error, 500} -> From ! {Ref, {500, "Internal Error"}};
        {error, 404} -> From ! {Ref, {404, "Not Found"}};
        {ok, ActionPid} ->  
            % perform action
            case actionModuleServer:action(ActionPid, Request, Enviroment) of
                {error, 500} -> From ! {Ref, {500, "Internal Error"}};
                {error, 404} -> From ! {Ref, {404, "Not Found"}};
                Content -> From ! {Ref, {200, Content}}
            end
    end,
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
