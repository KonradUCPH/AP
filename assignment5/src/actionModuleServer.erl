%%%-------------------------------------------------------------------
%%% This module turns an actionModule into a server
%%% based on the gen_server Example by Ken Friis Larsen (see below)
%%% @author Per Steffen Czolbe
%%%-------------------------------------------------------------------




%%%-------------------------------------------------------------------
%%% @author Ken Friis Larsen <ken@friislarsen.net>
%%% @copyright (C) 2017, Ken Friis Larsen
%%% @doc
%%%
%%% @end
%%% Created : 12 Oct 2017 by Ken Friis Larsen <ken@friislarsen.net>
%%%-------------------------------------------------------------------
-module(actionModuleServer).

%% needs action module with these callbacks:
-callback initialise(Arg :: term()) ->
    { ok, State :: term()} |
    { error, Reason :: term()}.
-callback action(Req :: term(), Env :: term(), State :: term()) ->
    {new_state, Content :: term(), NewState :: term()} |
    {no_change, Content :: term()}.

%% obeys gen_server behaviour
-behaviour(gen_server).

%% API
-export([start/2, action/3]).


%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% define ?SERVER = ?MODULE = modulename
-define(SERVER, ?MODULE).


%%%===================================================================
%%% API
%%%===================================================================

% ServerRef can be the Pid
action(ServerRef, Request, Enviroment) -> 
    gen_server:call(ServerRef, {action, Request, Enviroment}).

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start(ActionModule, Args) ->
    gen_server:start(?MODULE, {ActionModule, Args}, []).  % Module, args, options

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
init({ActionModule, Args}) ->
    case ActionModule:initialise(Args) of
        {ok, State} -> {ok, {ActionModule, State}};
        {error, Reason} -> {stop, Reason}
    end.

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
handle_call({action, Request, Enviroment}, _From, State) ->
    {ActionModule, ModuleState} = State,
    case ActionModule:action(Request, Enviroment, ModuleState) of
        {new_state, Content, NewState} -> {reply, Content, {ActionModule, NewState}};
        {no_change, Content} -> {reply, Content, State}
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
handle_cast(_Msg, State) ->
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
