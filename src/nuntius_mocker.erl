%%% @doc A process that mocks another one.
-module(nuntius_mocker).

-export([start_link/2]).
-export([mocked_process/1, history/1, reset_history/1, delete/1]).
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

%% @doc Reregisters the mocked process with its name, thus removing the mock.
-spec delete(nuntius:process_name()) -> ok.
delete(ProcessName) ->
    reregister(ProcessName, mocked_process(ProcessName)).

%% @doc Returns the PID of a mocked process (the original one with that name).
-spec mocked_process(nuntius:process_name()) -> pid().
mocked_process(ProcessName) ->
    {dictionary, Dict} = erlang:process_info(whereis(ProcessName), dictionary),
    {'$nuntius_mocker_mocked_process', ProcessPid} =
        lists:keyfind('$nuntius_mocker_mocked_process', 1, Dict),
    ProcessPid.

%% @doc Returns the history of messages received by a mocked process.
-spec history(nuntius:process_name()) -> [nuntius:event()].
history(ProcessName) ->
    {ok, History} = gen:call(ProcessName, '$nuntius_call', history),
    History.

%% @doc Erases the history for a mocked process.
%%      Note that there is no gen:cast(...),
%%      gen_server and others basically just send the message and move on, like us.
-spec reset_history(nuntius:process_name()) -> ok.
reset_history(ProcessName) ->
    ProcessName ! {'$nuntius_cast', reset_history},
    ok.

%% @private
-spec init(nuntius:process_name(), pid(), nuntius:opts()) -> no_return().
init(ProcessName, ProcessPid, Opts) ->
    ProcessMonitor = erlang:monitor(process, ProcessPid),
    reregister(ProcessName, self()),
    erlang:put('$nuntius_mocker_mocked_process', ProcessPid),
    proc_lib:init_ack({ok, self()}),
    loop(#{process_name => ProcessName,
           process_pid => ProcessPid,
           process_monitor => ProcessMonitor,
           history => [],
           opts => Opts}).

%% @todo Do stuff with the received messages instead of ignoring them.
%% @todo Verify if, on process termination, we need to do something more than just dying.
loop(State) ->
    #{process_monitor := ProcessMonitor,
      process_pid := ProcessPid,
      history := History} =
        State,
    NextState =
        receive
            {'DOWN', ProcessMonitor, process, ProcessPid, Reason} ->
                exit(Reason);
            {'$nuntius_call', From, history} ->
                gen:reply(From, History),
                State;
            {'$nuntius_cast', reset_history} ->
                State#{history := []};
            Message ->
                handle_message(Message, State)
        end,
    loop(NextState).

reregister(ProcessName, ProcessPid) ->
    erlang:unregister(ProcessName),
    erlang:register(ProcessName, ProcessPid),
    ok.

handle_message(_Message, #{opts := #{passthrough := false, history := false}} = State) ->
    State;
handle_message(Message, #{opts := #{passthrough := false, history := true}} = State) ->
    add_event(Message, State);
handle_message(Message, #{opts := #{passthrough := true, history := false}} = State) ->
    passthrough(Message, State),
    State;
handle_message(Message, #{opts := #{passthrough := true, history := true}} = State) ->
    passthrough(Message, State),
    add_event(Message, State).

passthrough(Message, #{process_pid := ProcessPid}) ->
    ProcessPid ! Message.

add_event(Message, State) ->
    maps:update_with(history,
                     fun(History) ->
                        [#{timestamp => erlang:system_time(), message => Message} | History]
                     end,
                     State).
