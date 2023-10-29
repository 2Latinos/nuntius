%% @doc The main interface for the system.
-module(nuntius).

-export([delete/1, mocked/0, new/1, new/2]).
-export([delete/2, expect/2, expect/3, expects/1]).
-export([history/1, received/2, reset_history/1]).
-export([mocked_process/0, passthrough/0, passthrough/1]).
-export([mocked_process/1]).
-export([start/0, stop/0]).

-type event() ::
    #{timestamp := integer(),
      message := message(),
      mocked := boolean(),
      with := term(),
      stack := term(),
      passed_through := boolean()}.
-type expect_fun() :: fun((_) -> _).
-type expect_id() :: reference() | expect_name().
-type expect_name() :: atom().
-type message() :: term().
-type opts() ::
    #{passthrough => boolean(),
      history => boolean(),
      exit_on_nomatch => boolean()}.
-type process_name() :: atom().

-export_type([event/0, expect_fun/0, expect_id/0]).
-export_type([expect_name/0, message/0, opts/0]).
-export_type([process_name/0]).

%% @doc Starts the application.
%% @equiv application:ensure_all_started(nuntius)
%% @todo Decide if we actually need to export this function or we can just use it within new/2.
-spec start() -> {ok, [module()]}.
start() ->
    application:ensure_all_started(nuntius).

%% @doc Stops the application.
-spec stop() -> ok.
stop() ->
    application:stop(nuntius).

%% @doc Injects a new mock process in front of the process with the provided name.<br />
%%     Returns an error if there is no process registered under that name.
%% @equiv new(ProcessName, #{})
-spec new(process_name()) -> ok | {error, not_found}.
new(ProcessName) ->
    new(ProcessName, #{}).

%% @doc Injects a new mock process in front of the process with the provided name.<br /><br />
%%      Options:<br />
%%          <ul>
%%          <li>
%%          <b><code>passthrough</code></b>: if <code>true</code>, all messages are passed through
%%              to the process by default.
%%              If false, messages that are not handled by any expectation are just dropped.
%%              <ul><li>default: <code>true</code></li></ul>
%%          </li>
%%          <li>
%%          <b><code>history</code></b>: if <code>true</code>, the mocking process will keep
%%          the history of messages received.
%%              <ul><li>default: <code>true</code></li></ul>
%%          </li>
%%          <b><code>exit_on_nomatch</code></b>: if <code>true</code>, the mocking process will
%%          exit with an exception if none of the expectations match the message. Otherwise it'll
%%          silently ignore it.
%%              <ul><li>default: <code>true</code></li></ul>
%%          </ul>
-spec new(process_name(), opts()) -> ok | {error, not_found}.
new(ProcessName, Opts) ->
    DefaultOpts =
        #{passthrough => true,
          history => true,
          exit_on_nomatch => true},
    nuntius_sup:start_mock(ProcessName, maps:merge(DefaultOpts, Opts)).

%% @doc Removes a mocking process.
-spec delete(process_name()) -> ok | {error, not_mocked}.
delete(ProcessName) ->
    nuntius_sup:stop_mock(ProcessName).

%% @doc Returns the list of mocked processes.
-spec mocked() -> [process_name()].
mocked() ->
    nuntius_sup:mocked().

%% @doc Returns the PID of a mocked process (the original one with that name).
-spec mocked_process(process_name()) -> pid() | {error, not_mocked}.
mocked_process(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_mocker:process/1).

%% @doc Passes the current message down to the mocked process.<br />
%% <b>Note</b>: this code should only be used inside an expect fun.
-spec passthrough() -> ok.
passthrough() ->
    nuntius_proc:passthrough().

%% @doc Passes a message down to the mocked process.<br />
%% <b>Note</b>: this code should only be used inside an expect fun.
-spec passthrough(message()) -> ok.
passthrough(Message) ->
    nuntius_proc:passthrough(Message).

%% @doc Returns the PID of the currently mocked process.<br />
%% <b>Note</b>: this code should only be used inside an expect fun.
-spec mocked_process() -> pid().
mocked_process() ->
    nuntius_proc:pid().

%% @doc Returns the history of messages received by a mocked process.
-spec history(process_name()) -> [event()] | {error, not_mocked}.
history(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_history:get/1).

%% @doc Returns whether a particular message was received already.<br />
%% <b>Note</b>: it only works with <code>history => true.</code>
-spec received(process_name(), message()) -> boolean() | {error, not_mocked}.
received(ProcessName, Message) ->
    if_mocked(ProcessName, fun(PN) -> nuntius_history:received(PN, Message) end).

%% @doc Erases the history for a mocked process.
-spec reset_history(process_name()) -> ok | {error, not_mocked}.
reset_history(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_history:reset/1).

%% @doc Adds a new expect function to a mocked process.<br />
%%      When a message is received by the process, this function will be run on it.<br />
%%      If the message doesn't match any clause, the process might exit,
%%      depending on option <code>exit_on_nomatch</code>.<br />
%%      If the process is not mocked, an error is returned.<br />
%%      When the function is successfully added, a reference is returned as an identifier.
-spec expect(process_name(), expect_fun()) -> reference() | {error, not_mocked}.
expect(ProcessName, Function) ->
    do_expect(ProcessName, erlang:make_ref(), Function).

%% @doc Adds a new <em>named</em> expect function to a mocked process.<br />
%%      When a message is received by the process, this function will be run on it.<br />
%%      If the message doesn't match any clause, the process might exit,
%%      depending on option <code>exit_on_nomatch</code>.<br />
%%      If the process is not mocked, an error is returned.<br />
%%      If there was already an expect function with that name, it's replaced.<br />
%%      When the expect function is successfully added or replaced, it'll keep the name
%%        as its identifier.
-spec expect(process_name(), expect_name(), expect_fun()) ->
                expect_name() | {error, not_mocked}.
expect(ProcessName, ExpectName, Function) ->
    do_expect(ProcessName, ExpectName, Function).

do_expect(ProcessName, ExpectId, Function) ->
    if_mocked(ProcessName,
              fun(PN) ->
                 nuntius_expect:save(PN, ExpectId, Function),
                 ExpectId
              end).

%% @doc Removes an expect function.<br />
%%      If the expect function was not already there, this function still returns 'ok'.<br />
%%      If the process is not mocked, an error is returned.
-spec delete(process_name(), expect_id()) -> ok | {error, not_mocked}.
delete(ProcessName, ExpectId) ->
    if_mocked(ProcessName, fun(PN) -> nuntius_expect:delete(PN, ExpectId) end).

%% @doc Returns the list of expect functions for a process.
-spec expects(process_name()) -> Expectations | {error, not_mocked}
    when Expectations :: #{expect_id() => expect_fun()}.
expects(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_expect:list/1).

if_mocked(ProcessName, Function) ->
    case lists:member(ProcessName, mocked()) of
        false ->
            {error, not_mocked};
        true ->
            Function(ProcessName)
    end.
