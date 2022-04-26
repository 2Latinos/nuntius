%% @hidden
-module(nuntius_mocker).

-export([call/2, cast/2]).
-export([delete/1, process/1]).
-export([init/3, start_link/2]).

%% @doc Sends an asynchronous request to the mocker.
-spec cast(nuntius:process_name(), nuntius:message()) -> ok.
cast(ProcessName, Message) ->
    ProcessName ! {'$nuntius_cast', Message},
    ok.

%% @doc Makes a synchronous request to the mocker.
-spec call(nuntius:process_name(), nuntius:message()) -> term().
call(ProcessName, Message) ->
    {ok, Result} = gen:call(ProcessName, '$nuntius_call', Message),
    Result.

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
    reregister(ProcessName, process(ProcessName)).

%% @doc Returns the PID of a mocked process (the original one with that name).
-spec process(nuntius:process_name()) -> pid().
process(ProcessName) ->
    {dictionary, Dict} = erlang:process_info(whereis(ProcessName), dictionary),
    {'$nuntius_mocker_process', ProcessPid} =
        lists:keyfind('$nuntius_mocker_process', 1, Dict),
    ProcessPid.

-spec init(nuntius:process_name(), pid(), nuntius:opts()) -> no_return().
init(ProcessName, ProcessPid, Opts) ->
    ProcessMonitor = erlang:monitor(process, ProcessPid),
    reregister(ProcessName, self()),
    erlang:put('$nuntius_mocker_process', ProcessPid),
    proc_lib:init_ack({ok, self()}),
    nuntius_proc:pid(ProcessPid),
    loop(#{process_name => ProcessName,
           process_monitor => ProcessMonitor,
           history => [],
           opts => Opts,
           expects => #{}}).

%% @todo Verify if, on process termination, we need to do something more than just dying.
loop(State) ->
    ProcessPid = nuntius_proc:pid(),
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
    nuntius_proc:current_message(Message),
    nuntius_proc:passed_through(false),
    ExpectsMatched = run_expects(Message, Expects),
    _ = maybe_passthrough(Message, ExpectsMatched, State),
    maybe_add_event({Message, ExpectsMatched}, State).

run_expects(Message, Expects) ->
    maps:fold(fun (_Id, _Expect, {{'$nuntius', match}, _} = Result) ->
                      Result;
                  (_Id, Expect, {'$nuntius', nomatch}) ->
                      try
                          {{'$nuntius', match}, Expect(Message)}
                      catch
                          error:function_clause ->
                              {'$nuntius', nomatch}
                      end
              end,
              {'$nuntius', nomatch},
              Expects).

maybe_passthrough(_Message, {{'$nuntius', match}, _}, _Opts) ->
    {'$nuntius', ignore};
maybe_passthrough(_Message, _ExpectsMatched, #{opts := #{passthrough := false}}) ->
    {'$nuntius', ignore};
maybe_passthrough(Message, _ExpectsMatched, _State) ->
    nuntius_proc:passthrough(Message).

maybe_add_event(_Message, #{opts := #{history := false}} = State) ->
    State;
maybe_add_event({Message, ExpectsMatched}, State) ->
    maps:update_with(history,
                     fun(History) ->
                        PassedThrough = nuntius_proc:passed_through(),
                        nuntius_proc:passed_through(false),
                        [#{timestamp => erlang:system_time(),
                           message => Message,
                           mocked => ExpectsMatched =/= {'$nuntius', nomatch},
                           passed_through => PassedThrough}
                         | History]
                     end,
                     State).
