%%% @doc application behaviour implementation.
-module(nuntius_app).

-behaviour(application).

-export([start/2, stop/1]).

%% @private
%% @todo Implement nuntius_sup and start it from here.
start(_StartType, _StartArgs) ->
    {ok, self()}.

%% @private
stop(_State) ->
    ok.
