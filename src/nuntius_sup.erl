%%% @doc Main supervisor.
-module(nuntius_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([mocked/0]).
-export([init/1]).
-export([start_mock/2]).

%% @private
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, nuntius_sup}, nuntius_sup, []).

%% @doc Returns the list of mocked processes.
-spec mocked() -> [nuntius:process_name()].
mocked() ->
    [ProcessName
     || {_, Pid, worker, _} <- supervisor:which_children(nuntius_sup),
        {registered_name, ProcessName} <- erlang:process_info(Pid, [registered_name])].

%% @private
init([]) ->
    {ok,
     {#{strategy => simple_one_for_one},
      [#{id => nuntius_mocker,
         restart => temporary,
         start => {nuntius_mocker, start_link, []}}]}}.

%% @doc Starts a new mock process.
%% @see nuntius:new/2
-spec start_mock(nuntius:process_name(), nuntius:opts()) -> ok | {error, not_found}.
start_mock(ProcessName, Opts) ->
    case supervisor:start_child(nuntius_sup, [ProcessName, Opts]) of
        {ok, undefined} ->
            {error, not_found};
        {ok, _} ->
            ok
    end.
