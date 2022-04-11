%%% @doc Tests for the top-level API.
-module(nuntius_api_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([default_mock/1, error_not_found/1]).

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

%% @doc The default mock is practically invisible.
default_mock(_Config) ->
    % Original state
    2 = add_one(1),
    % We mock the process but we don't handle any message
    ok = nuntius:new(plus_oner),
    % So, nothing changes
    2 = add_one(1),
    ok.

%% @doc If the process to mock doesn't exist, an error is raised.
%%      Unless the <pre>must_exist</pre> flag is specified.
error_not_found(_Config) ->
    % By default, you can't mock nonexisting processes
    {error, not_found} = nuntius:new(non_existing_process),
    % But you can, if you use <pre>must_exist => false</pre>
    ok = nuntius:new(non_exisiting_process, #{must_exist => false}),
    % Of course, you can mock existing processes using that flag, too
    ok = nuntius:new(plus_oner, #{must_exist => false}),
    ok.

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
