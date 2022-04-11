%%% @doc The main interface for the system.
-module(nuntius).

-export([start/0, stop/0]).
-export([new/1, new/2]).

-type process_name() :: atom().
-type opts() ::
    #{must_exist => boolean(),
      passthrough => boolean(),
      link => boolean(),
      history => boolean()}.

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
    new(ProcessName,
        #{must_exist => true,
          passthrough => true,
          link => true,
          history => true}).

%% @doc Injects a new mock process in front of the process with the provided name.
%%      Options:
%%          - <b>must_exist:</b> If true and the process doesn't exist, an error is returned.
%%              <b>Default:</b> <pre>true</pre>
%%              <b>Implies:</b> <pre>link => false</pre>
%%          - <b>passthrough:</b> If true, all messages are passed through
%%              to the process by default.
%%              If false, messages that are not handled by any expectation are just dropped.
%%              <b>Default:</b> <pre>true</pre>
%%          - <b>link:</b> If true, the mocking process is linked to the mocked one.
%%              <b>Default:</b> <pre>true</pre>
%%          - <b>history:</b> If true, the mocking process will keep the history of messages
%%              received.
%%              <b>Default:</b> <pre>true</pre>
-spec new(process_name(), opts()) -> ok | {error, not_found}.
new(ProcessName, Opts) ->
    nuntius_sup:start_mock(ProcessName, Opts).
