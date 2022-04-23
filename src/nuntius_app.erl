%% @hidden
%% @doc application behaviour implementation.
-module(nuntius_app).

-behaviour(application).

-export([start/2, stop/1]).

-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
    nuntius_sup:start_link().

-spec stop(_) -> ok.
stop(_) ->
    ok.
