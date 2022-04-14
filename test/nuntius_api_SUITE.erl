%%% @doc Tests for the top-level API.
-module(nuntius_api_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([default_mock/1, error_not_found/1, delete_mock/1, mocked_processes/1, history/1,
         no_history/1, expect/1, expect_message/1, mocked_process/1]).

-elvis([{elvis_style, dont_repeat_yourself, #{min_complexity => 13}}]).

all() ->
    [F
     || {F, 1} <- nuntius_api_SUITE:module_info(exports),
        not lists:member(F, [init_per_suite, end_per_suite, module_info])].

init_per_suite(Config) ->
    _ = nuntius:start(),
    Config.

end_per_suite(Config) ->
    Config.

init_per_testcase(_TestCase, Config) ->
    PlusOner = spawn(fun plus_oner/0),
    true = register(plus_oner, PlusOner),
    Echo = spawn(fun echo/0),
    true = register(echo, Echo),
    [{echo_pid, Echo}, {plus_oner_pid, PlusOner} | Config].

end_per_testcase(_TestCase, Config) ->
    {value, {plus_oner_pid, PlusOner}, ConfigWithoutPlusOner} =
        lists:keytake(plus_oner_pid, 1, Config),
    exit(PlusOner, kill),
    {value, {echo_pid, Echo}, CleanConfig} =
        lists:keytake(echo_pid, 1, ConfigWithoutPlusOner),
    exit(Echo, kill),
    CleanConfig.

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
error_not_found(_Config) ->
    % By default, you can't mock nonexisting processes
    {error, not_found} = nuntius:new(non_existing_process),
    ok.

%% @doc Processes can be unmocked.
delete_mock(Config) ->
    {plus_oner_pid, PlusOner} = lists:keyfind(plus_oner_pid, 1, Config),
    % Trying to remove a non-existent mock, fails
    {error, not_mocked} = nuntius:delete(plus_oner),
    {error, not_mocked} = nuntius:delete(doesnt_even_exist),
    PlusOner = whereis(plus_oner),
    % Mocking it and later deleting the mock
    % restores the registered name to the mocked process
    ok = nuntius:new(plus_oner),
    false = PlusOner == whereis(plus_oner),
    ok = nuntius:delete(plus_oner),
    PlusOner = whereis(plus_oner),
    % And the process is, again, not mocked
    {error, not_mocked} = nuntius:delete(plus_oner),
    ok.

%% @doc Users can get the pid of a mocked process and the list of all mocked processes
mocked_processes(Config) ->
    % Initially, no mocked processes
    [] = nuntius:mocked(),
    % If process is not mocked, you can't find the original process
    {error, not_mocked} = nuntius:mocked_process(plus_oner),
    % Once you mock it, you can get the original PID
    {plus_oner_pid, PlusOner} = lists:keyfind(plus_oner_pid, 1, Config),
    ok = nuntius:new(plus_oner),
    false = PlusOner == whereis(plus_oner),
    PlusOner = nuntius:mocked_process(plus_oner),
    % And the process appears in the list of mocked processes
    [plus_oner] = nuntius:mocked(),
    % If you mock two processes, they both appear in the list
    ok = nuntius:new(echo),
    [echo, plus_oner] =
        lists:sort(
            nuntius:mocked()),
    % If you remove a mock, it goes away from the list
    ok = nuntius:delete(plus_oner),
    [echo] = nuntius:mocked(),
    ok.

history(_Config) ->
    % If a process is not mocked, nuntius returns an error
    {error, not_mocked} = nuntius:history(plus_oner),
    {error, not_mocked} = nuntius:received(plus_oner, any_message),
    % We mock it
    ok = nuntius:new(plus_oner),
    % Originally the history is empty
    [] = nuntius:history(plus_oner),
    false = nuntius:received(plus_oner, 1),
    % We send a message to it
    2 = add_one(1),
    % The message appears in the history
    [#{timestamp := T1, message := {_, _, 1} = M1}] = nuntius:history(plus_oner),
    true = nuntius:received(plus_oner, M1),
    false = nuntius:received(plus_oner, 2),
    % We send another message
    3 = add_one(2),
    % The message appears in the history
    [#{timestamp := T1, message := M1}, #{timestamp := T2, message := M2}] =
        lists:sort(
            nuntius:history(plus_oner)),
    true = T1 < T2,
    true = nuntius:received(plus_oner, M1),
    true = nuntius:received(plus_oner, M2),
    % If we reset the history, it's now empty again
    ok = nuntius:reset_history(plus_oner),
    [] = nuntius:history(plus_oner),
    false = nuntius:received(plus_oner, M1),
    false = nuntius:received(plus_oner, M2),
    % We send yet another message
    4 = add_one(3),
    [#{timestamp := T3, message := M3}] = nuntius:history(plus_oner),
    true = T2 < T3,
    false = nuntius:received(plus_oner, M1),
    false = nuntius:received(plus_oner, M2),
    true = nuntius:received(plus_oner, M3),
    ok.

no_history(_Config) ->
    ok = nuntius:new(plus_oner, #{history => false}),
    % Originally the history is empty
    [] = nuntius:history(plus_oner),
    false = nuntius:received(plus_oner, 1),
    % We send a message to it
    2 = add_one(1),
    % The history is still empty
    [] = nuntius:history(plus_oner),
    false = nuntius:received(plus_oner, 1),
    % Resetting the history has no effect
    ok = nuntius:reset_history(plus_oner),
    [] = nuntius:history(plus_oner),
    false = nuntius:received(plus_oner, 1),
    % We send another message to it
    3 = add_one(2),
    % The history is still empty
    [] = nuntius:history(plus_oner),
    false = nuntius:received(plus_oner, 1),
    false = nuntius:received(plus_oner, 2),
    ok.

% @doc Your process can contain several expectations and you can
%      list them by reference too. We're not testing the handling of
%      messages, here.
expect(_Config) ->
    Expects = fun() -> nuntius:expects(plus_oner) end,
    ok = nuntius:new(plus_oner),
    % Add (unnamed) expectations...
    Ref1 = nuntius:expect(plus_oner, fun(_) -> ok end),
    true = is_reference(Ref1),
    Ref2 = nuntius:expect(plus_oner, fun(_) -> ok end),
    true = is_reference(Ref2),
    % ... and (only known) references in expectations
    2 = maps:size(Expects()),
    [Ref2] = maps:keys(Expects()) -- [Ref1],
    % Now add (named) expectations...
    named_exp1 = nuntius:expect(plus_oner, named_exp1, fun(_) -> ok end),
    % ... and check they're there
    3 = maps:size(Expects()),
    [named_exp1] = maps:keys(Expects()) -- [Ref1, Ref2],
    % ... and that using the same name overwrites existing expectations
    named_exp1 = nuntius:expect(plus_oner, named_exp1, fun(_) -> ok end),
    [named_exp1] = maps:keys(Expects()) -- [Ref1, Ref2],
    % ... though different names don't
    FunNamedExp2 = fun(_) -> ok end,
    named_exp2 = nuntius:expect(plus_oner, named_exp2, FunNamedExp2),
    4 = maps:size(Expects()),
    [named_exp2] = maps:keys(Expects()) -- [Ref1, Ref2, named_exp1],
    % Let's now delete an expectation...
    {ok, _} = maps:find(Ref1, Expects()), % It was here...
    ok = nuntius:delete(plus_oner, Ref1),
    error = maps:find(Ref1, Expects()), % ... and it's not anymore
    % ... and another one
    {ok, _} = maps:find(Ref2, Expects()), % It was here...
    ok = nuntius:delete(plus_oner, Ref2),
    error = maps:find(Ref2, Expects()), % ... and it's not anymore
    % ... and another one
    {ok, _} = maps:find(named_exp1, Expects()), % It was here...
    ok = nuntius:delete(plus_oner, named_exp1),
    error = maps:find(named_exp1, Expects()), % ... and it's not anymore
    % named_exp2 is still there (with it's function)
    1 = maps:size(Expects()),
    #{named_exp2 := _} = Expects().

expect_message(_Config) ->
    ok = nuntius:new(echo, #{passthrough => false}),
    Self = self(),
    % Have the expectation send a message back to us
    _ = nuntius:expect(echo, boom_echo, fun(boomerang = M) -> Self ! {echoed, M} end),
    _ = nuntius:expect(echo, kyli_echo, fun(kylie = M) -> Self ! {echoed, M} end),
    echo ! boomerang,
    receive
        {echoed, boomerang} ->
            ok
    after 250 ->
        error(timeout)
    end,
    % Check if a nonmatching expectation would also work
    echo ! unknown,
    ok =
        receive
            _ ->
                ignored
        after 250 ->
            ok
        end.

mocked_process(_Config) ->
    Pid = whereis(echo),
    Self = self(),
    ok = nuntius:new(echo),
    _ = nuntius:expect(echo, fun (boom) -> Pid = nuntius:mocked_process(), Self ! from_mocked end),
    % We send the mocked process a message
    echo ! boom,
    ok = receive
        from_mocked ->
            ok % ... and if we got here we have echo's Pid inside the expectation
    after 250 ->
        error(timeout)
    end.

add_one(ANumber) ->
    call(plus_oner, ANumber).

call(Process, Message) ->
    Ref = make_ref(),
    Process ! {self(), Ref, Message},
    receive
        {Ref, Result} ->
            Result
    after 1000 ->
        error(#{reason => timeout,
                process => Process,
                message => Message})
    end.

plus_oner() ->
    receive
        {Caller, Ref, ANumber} ->
            Caller ! {Ref, ANumber + 1}
    end,
    plus_oner().

echo() ->
    receive
        {Caller, Ref, Message} ->
            Caller ! {Ref, Message}
    end,
    echo().
