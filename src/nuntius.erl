%%% @doc The main interface for the system.
-module(nuntius).

-export([start/0, stop/0]).
-export([new/1, new/2, delete/1]).
-export([mocked/0, mocked_process/1]).
-export([history/1, received/2, reset_history/1]).

-type process_name() :: atom().
-type event() :: #{timestamp := integer(), message := term()}.
-type opts() :: #{passthrough => boolean(), history => boolean()}.

-export_type([process_name/0, opts/0, event/0]).

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

%% @doc Injects a new mock process in front of the process with the provided name.
%%     Returns an error if there is no process registered under that name.
%% @equiv new(ProcessName, #{})
-spec new(process_name()) -> ok | {error, not_found}.
new(ProcessName) ->
    new(ProcessName, #{}).

%% @doc Injects a new mock process in front of the process with the provided name.
%%      Options:
%%          - <b>passthrough:</b> If true, all messages are passed through
%%              to the process by default.
%%              If false, messages that are not handled by any expectation are just dropped.
%%              <b>Default:</b> <pre>true</pre>
%%          - <b>history:</b> If true, the mocking process will keep the history of messages
%%              received.
%%              <b>Default:</b> <pre>true</pre>
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

%% @doc Returns the history of messages received by a mocked process.
-spec history(process_name()) -> [event()] | {error, not_mocked}.
history(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_mocker:history/1).

%% @doc Returns whether a particular message was received already.
%%
%% <em>Note: it only works with <pre>history => true.</pre></em>
-spec received(process_name(), term()) -> boolean() | {error, not_mocked}.
received(ProcessName, Message) ->
    if_mocked(ProcessName, fun(PN) -> nuntius_mocker:received(PN, Message) end).

%% @doc Erases the history for a mocked process.
-spec reset_history(process_name()) -> ok | {error, not_mocked}.
reset_history(ProcessName) ->
    if_mocked(ProcessName, fun nuntius_mocker:reset_history/1).

if_mocked(ProcessName, Function) ->
    case lists:member(ProcessName, mocked()) of
        false ->
            {error, not_mocked};
        true ->
            Function(ProcessName)
    end.
