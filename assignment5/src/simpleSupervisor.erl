%% Simple supervisor. Does not keep track of child tsate, child is restarted in initial state.
-module(simpleSupervisor).
-export([start/2]).

%% Interface

% starts the supervisor, which in turn starts the given module with the args
% returns Pid used to communicate with the worker
start(Module, Args) -> 
    % spawn worker
    Worker = Module:start_link(Args),
    % spawn supervisor
    spawnSupervisor(Worker).

% used by worker to back up state
backup(SupervisorPid, State) ->
    async(SupervisorPid, {backup, State}),
    ok.


%% Internal implementation
async(Pid, Msg) ->
    Pid ! {self(), Msg}.

spawnSupervisor(WorkerPid) ->
    spawn(fun () ->
                  process_flag(trap_exit, true),,
                  supervisor(WorkerPid, nothing, nothing)
          end).

supervisor(SlavePid, LastMsg, SlaveState) ->
    receive
        % worker died
        {'EXIT', SlavePid, Reason} ->
            io:format("~p exited because of ~p~n", [SlavePid, Reason]),
            Daddy = self(),
            Pid1 = spawn_link(fun() -> loop(Daddy, SlaveState) end),
            case LastMsg of
                {From, _} ->
                    From ! {self(), worker_died};
                _ -> nothing
            end,
            supervisor(Pid1, empty_msg_or_something, SlaveState);
        % worker wants to backup state
        {_From, {backup, State}} ->
            supervisor(SlavePid, LastMsg, State);
        % someone wants to msg worker
        Msg ->
            SlavePid ! Msg,
            supervisor(SlavePid, Msg, SlaveState)
    end.