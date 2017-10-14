%% Simple supervisor. Does not keep track of child state, child is restarted in initial state.
%% inc ase of worker crash, send message {SupervisorPid, worker_died} to last message sender
-module(simpleSupervisor).
-export([start/2]).

%% needs a worker module with these callbacks:
-callback start_link(Args :: term()) ->
    { ok, State :: term()} |
    { error, Reason :: term()}.


%% Interface

% starts the supervisor, which in turn starts the given module with the args
% returns Pid used to communicate with the worker
start(WorkerModule, WorkerArgs) -> 
    SPid = spawn(fun () ->
                    process_flag(trap_exit, true),
                    {ok, WorkerPid} = spawnWorker(WorkerModule, WorkerArgs),
                    supervisor(WorkerPid, nothing, WorkerModule, WorkerArgs)
                end),
    case SPid of
        [] -> {error, failed_to_spawn_supervisor};
        _ -> {ok, SPid}
    end.


%% Internal implementation

spawnWorker(Module, Args) ->
    Module:start_link(Args).


supervisor(SlavePid, LastMsg, RestartModule, RestartArgs) ->
    receive
        % worker died
        {'EXIT', SlavePid, Reason} ->
            io:format("~p exited because of ~p~n", [SlavePid, Reason]),
            {ok, Pid} = spawnWorker(RestartModule, RestartArgs),
            case LastMsg of
                {_, {From, _}} ->
                    From ! {self(), worker_died};
                Message -> io:format("Supervisor coudnt decipher msg ~p~n", [Message])
            end,
            supervisor(Pid, nothing, RestartModule, RestartArgs);
        % someone wants to msg worker
        Msg ->
            SlavePid ! Msg,
            supervisor(SlavePid, Msg, RestartModule, RestartArgs)
    end.