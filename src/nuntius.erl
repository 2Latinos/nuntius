%%% @doc The main interface for the system.
-module(nuntius).

-export([start/0, stop/0]).

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
