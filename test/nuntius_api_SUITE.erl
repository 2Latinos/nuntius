%%% @doc Tests for the top-level API.
-module(nuntius_api_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([passthrough_mock_has_no_effect/1]).

all() ->
    [F
     || {F, 1} <- nuntius_api_SUITE:module_info(exports),
        not lists:member(F, [init_per_suite, end_per_suite, module_info])].

init_per_suite(Config) ->
    nuntius:start(),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    PlusOner = spawn(fun plus_oner/0),
    true = register(plus_oner, PlusOner),
    [{plus_oner_pid, PlusOner} | Config].

end_per_testcase(_TestCase, Config) ->
    {plus_oner_pid, PlusOner} = lists:keyfind(plus_oner_pid, 1, Config),
    exit(PlusOner, kill),
    Config.

passthrough_mock_has_no_effect(_Config) ->
    % Original state
    2 = add_one(1),
    % We mock the process but we don't handle any message
    ok = nuntius:new(plus_oner),
    % So, nothing changes
    2 = add_one(1),
    {comment, ""}.

plus_oner() ->
    receive
        {Caller, Ref, ANumber} ->
            Caller ! {Ref, ANumber + 1}
    end,
    plus_oner().

add_one(ANumber) ->
    Ref = make_ref(),
    plus_oner ! {self(), Ref, ANumber},
    receive
        {Ref, Result} ->
            Result
    after 1000 ->
        error(#{reason => timeout, parameter => ANumber})
    end.
