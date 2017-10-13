-module(supervisedActionModuleServer).
-export([start/2]).

% spawns a supervisor process and calls start() on the worker
start(ActionModule, Args) ->
    SupervisorPid = spawn(fun () ->
                            process_flag(trap_exit, true),
                            {ok, State} = ActionModule:initialise(Args),
                            Supervisor = self(),
                            {ok, WorkerPid} = actionModuleServer:start(ActionModule, {Args, Supervisor}),
                            supervisor(WorkerPid, nothing, State)
                    end),
    {ok, SupervisorPid}.

supervisor(WorkerPid, LastMsg, SlaveState) ->
    receive
        {'EXIT', Pid, Reason} ->
            io:format("~p exited because of ~p~n", [Pid, Reason]),
            {ok, WorkerPid1} = actionModuleServer:start(SlaveState),
            case LastMsg of
                {From, Ref, _Request} ->
                    From ! {Ref, 500};
                _ -> nothing
            end,
            supervisor(WorkerPid1, nothing, SlaveState);
        {backup, State} ->
            supervisor(WorkerPid, LastMsg, State);
        Msg ->
            WorkerPid ! Msg,
            supervisor(WorkerPid, Msg, SlaveState)
    end.