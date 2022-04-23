%% @doc The main interface for the system.
-module(nuntius).

-export([start/0, stop/0]).
-export([new/1, new/2, delete/1]).
-export([mocked/0, mocked_process/1]).
-export([passthrough/0, passthrough/1, mocked_process/0]).
-export([history/1, received/2, reset_history/1]).
-export([expect/2, expect/3, delete/2, expects/1]).

-type process_name() :: atom().
-type event() :: #{timestamp := integer(), message := term()}.
-type opts() :: #{passthrough => boolean(), history => boolean()}.
-type expect_fun() :: fun((_) -> _).
-type expect_name() :: atom().
-type expect_id() :: reference() | expect_name().

-export_type([process_name/0, opts/0, event/0]).
-export_type([expect_fun/0, expect_name/0, expect_id/0]).

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
%%          </ul>
-spec new(process_name(), opts()) -> ok | {error, not_found}.
new(ProcessName, Opts) ->
    DefaultOpts = #{passthrough => true, history => true},
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
    if_mocked(ProcessName, fun nuntius_mocker:mocked_process/1).

%% @doc Passes the current message down to the mocked process.<br />
%% <b>Note</b>: this code should only be used inside an expect fun.
-spec passthrough() -> ok.
passthrough() ->
    nuntius_mocker:passthrough().

%% @doc Passes a message down to the mocked process.<br />
%% <b>Note</b>: this code should only be used inside an expect fun.
-spec passthrough(term()) -> ok.
passthrough(Message) ->
    nuntius_mocker:passthrough(Message).

%% @doc Returns the PID of the currently mocked process.<br />
%% <b>Note</b>: this code should only be used inside an expect fun.
-spec mocked_process() -> pid().
mocked_process() ->
    nuntius_mocker:mocked_process().

%% @doc Returns the history of messages received by a mocked process.
-spec history(process_name()) -> [event()] | {error, not_mocked}.
history(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_mocker:history/1).

%% @doc Returns whether a particular message was received already.<br />
%% <b>Note</b>: it only works with <code>history => true.</code>
-spec received(process_name(), term()) -> boolean() | {error, not_mocked}.
received(ProcessName, Message) ->
    if_mocked(ProcessName, fun(PN) -> nuntius_mocker:received(PN, Message) end).

%% @doc Erases the history for a mocked process.
-spec reset_history(process_name()) -> ok | {error, not_mocked}.
reset_history(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_mocker:reset_history/1).

%% @doc Adds a new expect function to a mocked process.<br />
%%      When a message is received by the process, this function will be run on it.<br />
%%      If the message doesn't match any clause, nothing will be done.<br />
%%      If the process is not mocked, an error is returned.<br />
%%      When the function is successfully added, a reference is returned as an identifier.
-spec expect(process_name(), expect_fun()) -> reference() | {error, not_mocked}.
expect(ProcessName, Function) ->
    do_expect(ProcessName, erlang:make_ref(), Function).

%% @doc Adds a new <em>named</em> expect function to a mocked process.<br />
%%      When a message is received by the process, this function will be run on it.<br />
%%      If the message doesn't match any clause, nothing will be done.<br />
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
                 nuntius_mocker:expect(PN, ExpectId, Function),
                 ExpectId
              end).

%% @doc Removes an expect function.<br />
%%      If the expect function was not already there, this function still returns 'ok'.<br />
%%      If the process is not mocked, an error is returned.
-spec delete(process_name(), expect_id()) -> ok | {error, not_mocked}.
delete(ProcessName, ExpectId) ->
    if_mocked(ProcessName, fun(PN) -> nuntius_mocker:delete(PN, ExpectId) end).

%% @doc Returns the list of expect functions for a process.
-spec expects(process_name()) -> Expectations | {error, not_mocked}
    when Expectations :: #{expect_id() => expect_fun()}.
expects(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_mocker:expects/1).

if_mocked(ProcessName, Function) ->
    case lists:member(ProcessName, mocked()) of
        false ->
            {error, not_mocked};
        true ->
            Function(ProcessName)
    end.
