%%% @doc Main supervisor.
-module(nuntius_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

%% @private
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    supervisor:start_link({local, nuntius_sup}, nuntius_sup, []).

%% @private
init([]) ->
    {ok,
     {#{strategy => simple_one_for_one},
      [#{id => nuntius_mocker, start => {nuntius_mocker, start_link, []}}]}}.
