-module(nuntius_SUITE).

-behaviour(ct_suite).

-export([all/0]).
-export([app_start_stop/1]).

all() ->
    [app_start_stop].

app_start_stop(_) ->
    {ok, Apps} = nuntius:start(),
    [nuntius | _] = Apps,
    {ok, []} = nuntius:start(),
    ok = nuntius:stop(),
    {ok, [nuntius]} = nuntius:start(),
    ok = nuntius:stop().
