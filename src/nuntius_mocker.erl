%%% @doc A process that mocks another one.
-module(nuntius_mocker).

-export([start_link/2]).
-export([mocked_process/1]).
-export([init/3]).

%% @doc Boots up a mocking process.
%%      If the process to be mocked doesn't exist, returns <pre>ignore</pre>.
-spec start_link(nuntius:process_name(), nuntius:opts()) -> {ok, pid()} | ignore.
start_link(ProcessName, Opts) ->
    case whereis(ProcessName) of
        undefined ->
            ignore;
        ProcessPid ->
            proc_lib:start_link(nuntius_mocker, init, [ProcessName, ProcessPid, Opts])
    end.

%% @doc Returns the PID of a mocked process (the original one with that name).
-spec mocked_process(nuntius:process_name()) -> pid().
mocked_process(ProcessName) ->
    {dictionary, Dict} = erlang:process_info(whereis(ProcessName), dictionary),
    {'$nuntius_mocker_mocked_process', ProcessPid} =
        lists:keyfind('$nuntius_mocker_mocked_process', 1, Dict),
    ProcessPid.

%% @private
-spec init(nuntius:process_name(), pid(), nuntius:opts()) -> no_return().
init(ProcessName, ProcessPid, Opts) ->
    ProcessMonitor = erlang:monitor(process, ProcessPid),
    erlang:unregister(ProcessName),
    erlang:register(ProcessName, self()),
    erlang:put('$nuntius_mocker_mocked_process', ProcessPid),
    proc_lib:init_ack({ok, self()}),
    loop(#{process_name => ProcessName,
           process_pid => ProcessPid,
           process_monitor => ProcessMonitor,
           opts => Opts}).

%% @todo Do stuff with the received messages instead of ignoring them.
%% @todo Collect message history if <pre>history := true</pre>.
%% @todo Verify if, on process termination, we need to do something more than just dying.
loop(State) ->
    #{process_monitor := ProcessMonitor, process_pid := ProcessPid} = State,
    receive
        {'DOWN', ProcessMonitor, process, ProcessPid, Reason} ->
            exit(Reason);
        Message ->
            case State of
                #{opts := #{passthrough := false}} ->
                    %% We don't pass messages through, we just ignore all of them.
                    ignore;
                #{process_pid := undefined} ->
                    %% We don't know where that process lives, so we ignore the message.
                    ignore;
                #{process_pid := ProcessPid} ->
                    %% We pass the message through.
                    ProcessPid ! Message
            end,
            loop(State)
    end.
