%%% @doc A process that mocks another one.
-module(nuntius_mocker).

-export([start_link/2]).
-export([init/3]).

%% @doc Boots up a mocking process.
%%      If the process to be mocked doesn't exist, returns <pre>ignore</pre>,
%%      unless <pre>must_exist</pre> is <pre>false</pre>.
-spec start_link(nuntius:process_name(), nuntius:opts()) -> {ok, pid()} | ignore.
start_link(ProcessName, Opts) ->
    case {whereis(ProcessName), Opts} of
        {undefined, #{must_exist := true}} ->
            ignore;
        {ProcessPid, Opts} ->
            proc_lib:start_link(nuntius_mocker, init, [ProcessName, ProcessPid, Opts])
    end.

%% @private
-spec init(nuntius:process_name(), pid() | undefined, nuntius:opts()) -> no_return().
init(ProcessName, ProcessPid, Opts) ->
    maybe_link(ProcessPid, Opts),
    maybe_unregister(ProcessName, ProcessPid),
    erlang:register(ProcessName, self()),
    proc_lib:init_ack({ok, self()}),
    loop(#{process_name => ProcessName,
           process_pid => ProcessPid,
           opts => Opts}).

%% @todo Do stuff with the received messages instead of ignoring them
loop(State) ->
    receive
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

maybe_unregister(_ProcessName, undefined) ->
    false;
maybe_unregister(ProcessName, _) ->
    erlang:unregister(ProcessName).

maybe_link(undefined, _) ->
    false;
maybe_link(_, #{link := false}) ->
    false;
maybe_link(ProcessPid, _) ->
    erlang:link(ProcessPid).
