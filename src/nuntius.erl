%%% @doc The main interface for the system.
-module(nuntius).

-export([start/0, stop/0]).
-export([new/1, new/2]).
-export([mocked/0, mocked_process/1]).

-type process_name() :: atom().
-type opts() :: #{passthrough => boolean(), history => boolean()}.

-export_type([process_name/0, opts/0]).

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

%% @doc Returns the list of mocked processes.
-spec mocked() -> [process_name()].
mocked() ->
    [ProcessName
     || {_, Pid, worker, _} <- supervisor:which_children(nuntius_sup),
        {registered_name, ProcessName} <- erlang:process_info(Pid, [registered_name])].

%% @doc Returns the PID of a mocked process (the original one with that name).
-spec mocked_process(process_name()) -> pid() | {error, not_mocked}.
mocked_process(ProcessName) ->
    case lists:member(ProcessName, mocked()) of
        false ->
            {error, not_mocked};
        true ->
            nuntius_mocker:mocked_process(ProcessName)
    end.
