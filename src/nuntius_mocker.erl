%%% @doc A process that mocks another one.
-module(nuntius_mocker).

-export([start_link/2]).
-export([mocked_process/1, history/1, received/2, reset_history/1, delete/1]).
-export([passthrough/0, passthrough/1, mocked_process/0]).
-export([expect/3, delete/2, expects/1]).
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

%% @doc Passes the current message down to the mocked process.
%%
%% <strong> Note </strong>: this code should only be used inside an expect fun.
-spec passthrough() -> ok.
passthrough() ->
    passthrough(current_message()).

%% @doc Passes a message down to the mocked process.
%%
%% <strong> Note </strong>: this code should only be used inside an expect fun.
-spec passthrough(term()) -> ok.
passthrough(Message) ->
    ProcessPid = process_pid(),
    ProcessPid ! Message.

%% @doc Returns the PID of the currently mocked process.
%%
%% <strong> Note </strong>: this code should only be used inside an expect fun.
-spec mocked_process() -> pid().
mocked_process() ->
    process_pid().

%% @doc Returns the history of messages received by a mocked process.
-spec history(nuntius:process_name()) -> [nuntius:event()].
history(ProcessName) ->
    {ok, History} = gen:call(ProcessName, '$nuntius_call', history),
    History.

%% @doc Returns whether a particular message was received already.
%%
%% <strong> Note </strong>: it only works with <pre>history => true.</pre>
-spec received(nuntius:process_name(), term()) -> boolean().
received(ProcessName, Message) ->
    {ok, Result} = gen:call(ProcessName, '$nuntius_call', {received, Message}),
    Result.

%% @doc Erases the history for a mocked process.
%%      Note that there is no gen:cast(...),
%%      gen_server and others basically just send the message and move on, like us.
-spec reset_history(nuntius:process_name()) -> ok.
reset_history(ProcessName) ->
    ProcessName ! {'$nuntius_cast', reset_history},
    ok.

%% @doc Adds a new expect function to a mocked process.
%%      When a message is received by the process, this function will be run on it.
%%      If the message doesn't match any clause, nothing will be done.
%%      If the process is not mocked, an error is returned.
%%      If the expect is named, and there was already an expect function with that name,
%%        it's replaced.
%%      When the expect function is successfully added or replaced:
%%        - if named, it'll keep the name as its identifier,
%%        - otherwise, a reference is returned as an identifier
-spec expect(nuntius:process_name(), nuntius:expect_id(), nuntius:expect_fun()) -> ok.
expect(ProcessName, ExpectId, Function) ->
    ProcessName ! {'$nuntius_cast', {expect, Function, ExpectId}},
    ok.

%% @doc Removes an expect function.
%%      If the expect function was not already there, this function still returns 'ok'.
%%      If the process is not mocked, an error is returned.
-spec delete(nuntius:process_name(), nuntius:expect_id()) -> ok.
delete(ProcessName, ExpectId) ->
    ProcessName ! {'$nuntius_cast', {delete, ExpectId}},
    ok.

%% @doc Returns the list of expect functions for a process.
-spec expects(nuntius:process_name()) ->
                 {ok, #{nuntius:expect_id() => nuntius:expect_fun()}}.
expects(ProcessName) ->
    {ok, Result} = gen:call(ProcessName, '$nuntius_call', expects),
    Result.

%% @private
-spec init(nuntius:process_name(), pid(), nuntius:opts()) -> no_return().
init(ProcessName, ProcessPid, Opts) ->
    ProcessMonitor = erlang:monitor(process, ProcessPid),
    reregister(ProcessName, self()),
    erlang:put('$nuntius_mocker_mocked_process', ProcessPid),
    proc_lib:init_ack({ok, self()}),
    process_pid(ProcessPid),
    loop(#{process_name => ProcessName,
           process_monitor => ProcessMonitor,
           history => [],
           opts => Opts,
           expects => #{}}).

%% @todo Verify if, on process termination, we need to do something more than just dying.
loop(State) ->
    ProcessPid = process_pid(),
    #{process_monitor := ProcessMonitor} = State,
    NextState =
        receive
            {'DOWN', ProcessMonitor, process, ProcessPid, Reason} ->
                exit(Reason);
            {'$nuntius_call', From, Call} ->
                _ = gen:reply(From, handle_call(Call, State)),
                State;
            {'$nuntius_cast', Cast} ->
                handle_cast(Cast, State);
            Message ->
                handle_message(Message, State)
        end,
    loop(NextState).

reregister(ProcessName, ProcessPid) ->
    erlang:unregister(ProcessName),
    erlang:register(ProcessName, ProcessPid),
    ok.

handle_call(history, #{history := History}) ->
    lists:reverse(History);
handle_call({received, Message}, #{history := History}) ->
    lists:any(fun(#{message := M}) -> M =:= Message end, History);
handle_call(expects, #{expects := Expects}) ->
    Expects.

handle_cast(reset_history, State) ->
    State#{history := []};
handle_cast({expect, Function, ExpectId}, #{expects := Expects} = State) ->
    State#{expects => Expects#{ExpectId => Function}};
handle_cast({delete, ExpectId}, #{expects := Expects} = State) ->
    State#{expects => maps:remove(ExpectId, Expects)}.

handle_message(Message, #{expects := Expects} = State) ->
    current_message(Message),
    ExpectsRan = run_expects(Message, Expects),
    ExpectsRan orelse maybe_passthrough(Message, State),
    maybe_add_event(Message, State).

run_expects(Message, Expects) ->
    maps:fold(fun (_Id, _Expect, true = _Done) ->
                      true;
                  (_Id, Expect, false) ->
                      try
                          Expect(Message),
                          true
                      catch
                          error:function_clause ->
                              false
                      end
              end,
              false,
              Expects).

maybe_passthrough(_Message, #{opts := #{passthrough := false}}) ->
    ignore;
maybe_passthrough(Message, _State) ->
    ProcessPid = process_pid(),
    ProcessPid ! Message.

maybe_add_event(_Message, #{opts := #{history := false}} = State) ->
    State;
maybe_add_event(Message, State) ->
    maps:update_with(history,
                     fun(History) ->
                        [#{timestamp => erlang:system_time(), message => Message} | History]
                     end,
                     State).

process_pid(ProcessPid) ->
    put(process_pid, ProcessPid).

process_pid() ->
    get(process_pid).

current_message(Message) ->
    put(current_message, Message).

current_message() ->
    get(current_message).
