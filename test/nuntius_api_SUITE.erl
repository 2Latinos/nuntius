%% @doc Tests for the top-level API.
-module(nuntius_api_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([default_mock/1, error_not_found/1, delete_mock/1, mocked_processes/1, history/1,
         no_history/1, expect/1, expect_message/1, mocked_process/1, passthrough/1,
         passthrough_message/1, run_expects_function_clause/1, exit_on_error_inside_expectation/1,
         exit_on_nomatch/1]).

-elvis([{elvis_style, dont_repeat_yourself, #{min_complexity => 13}}]).

all() ->
    [F
     || {F, 1} <- nuntius_api_SUITE:module_info(exports),
        not lists:member(F, [init_per_suite, end_per_suite, module_info])].

init_per_suite(Config) ->
    _ = nuntius:start(),
    Config.

end_per_suite(Config) ->
    nuntius:stop(),
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
        lists:sort(fun(#{timestamp := Left}, #{timestamp := Right}) -> Left =< Right end,
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
            [#{mocked := true, passed_through := false}] = nuntius:history(echo),
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
            [_, #{mocked := false, passed_through := false}] = nuntius:history(echo),
            ok
        end.

% @doc We get the mocked process id from the mock process
%      We play with message passing for extra guarantees
mocked_process(_Config) ->
    Pid = whereis(echo),
    Self = self(),
    ok = nuntius:new(echo),
    _ = nuntius:expect(echo,
                       fun(boom) ->
                          Pid = nuntius:mocked_process(),
                          Self ! from_mocked
                       end),
    % We send the mocked process a message
    echo ! boom,
    receive
        from_mocked ->
            [#{mocked := true, passed_through := false}] = nuntius:history(echo),
            ok % ... and if we got here we have echo's Pid inside the expectation
    after 250 ->
        error(timeout)
    end.

% @doc We pass the expectation message through to the mocked process
%      We play with message passing for extra guarantees
passthrough(_Config) ->
    ok = nuntius:new(echo),
    % We pass a message to the mocked process
    _ = nuntius:expect(echo, fun(_) -> nuntius:passthrough() end),
    call(echo, back), % ... and since it's an echo process, we get it back
    receive
        {_, back} ->
            error(received) % ... but not from the pass through
    after 250 ->
        % last message was explicitly passed through
        [#{mocked := true, passed_through := true}] = nuntius:history(echo),
        ok
    end.

% @doc We pass a specific message (from the expectation) through to the mocked
%        process
%      We play with message passing for extra guarantees
passthrough_message(_Config) ->
    ok = nuntius:new(echo, #{passthrough => false}),
    _ = nuntius:expect(echo,
                       fun(message) ->
                          % We pass a specific message to the mocked process
                          nuntius:passthrough({self(), make_ref(), message}),
                          receive
                              {_, message} ->
                                  ok % ... and then we get it back (inside the process)
                          after 250 ->
                              error(timeout)
                          end
                       end),
    echo ! message,
    % And now, for a different test...
    Self = self(),
    _ = nuntius:expect(echo,
                       fun(new_message) ->
                          % We pass another specific message to the mocked process
                          Mocked = nuntius:mocked_process(),
                          nuntius:passthrough({Self, make_ref(), {new_message, Mocked}})
                       end),
    echo ! new_message,
    Mocked = nuntius:mocked_process(echo),
    receive
        {_, {new_message, Mocked}} ->
            % ... and we get it back (from the mocking process - notice Mocked)
            [#{mocked := true, passed_through := true}, #{mocked := true, passed_through := true}] =
                nuntius:history(echo)
    after 250 ->
        error(not_received)
    end.

% @doc We make a real effort to distinguish a function_clause issued internally from a function
% clause issued by consumer code. This test makes sure that reality is constant.
-dialyzer([{nowarn_function, run_expects_function_clause/1}]).

run_expects_function_clause(_Config) ->
    % We make sure that a non matching function head will not provoke a test error.
    ok = nuntius:new(echo),
    _ = nuntius:expect(echo, fun(match_nothing_else) -> ok end),
    echo ! match_this,
    true = is_process_alive(whereis(echo)), % it lives
    receive
        _ ->
            ok
    after 250 ->
        ok
    end,
    true = is_process_alive(whereis(echo)), % it lives
    % ... and now we test the "opposite" (a consumer function_clause).
    _ = nuntius:expect(echo,
                       fun(_) ->
                          lists:sort(fff) % this provokes a function_clause
                       end),
    echo ! sort_it,
    true = is_process_alive(whereis(echo)), % it lives
    receive
        _ ->
            ok
    after 250 ->
        ok
    end,
    undefined = whereis(echo). % it no longer lives

    % using e.g. echo ! 2 shows that what killed the process was {lists,sort,[fff].

exit_on_error_inside_expectation(_Config) ->
    % We make sure that an error inside an expectation will be visible to the consumer.
    ok = nuntius:new(plus_oner),
    _ = nuntius:expect(plus_oner, fun ({_Self, _Ref, 1}) -> lists:sort(fff) end),

    % Output'll be something like:
    % exception exit: {nuntius,nomatch,
    %                     [{lists,sort,[fff],[{file,"lists.erl"},{line,512}]},
    %                      {nuntius_mocker,'-run_expects/3-fun-1-',3,
    %                          [{file,
    %                               "/home/user/nuntius/src/nuntius_mocker.erl"},
    %                           {line,108}]}
    % The error is inside the expectation so it's "less" visible.
    try
        add_one(1),
        exit(wont_get_here)
    catch error:_ -> % You have to remove this to see how the consumer would see it
        ok
    end.

exit_on_nomatch(_Config) ->
    % We make sure that a non matching function head will throw an exception.
    ok = nuntius:new(plus_oner),
    _ = nuntius:expect(plus_oner, fun ({_Self, _Ref, 14}) -> not_matched end),

    % Output'll be something like:
    % exception exit: {nuntius,nomatch,
    %                     [{nuntius_api_SUITE,'-exit_on_nomatch/1-fun-0-',
    %                          [{<0.551.0>,#Ref<0.3533323057.4076863489.191396>,
    %                            13}],
    %                          [{file,
    %                               "/home/user/nuntius/test/nuntius_api_SUITE.erl"},
    %                           {line,350}]}
    % The error is in the expectation head so it's visible.
    try
        add_one(13),
        exit(wont_get_here)
    catch error:_ -> % You have to remove this to see how the consumer would see it
        ok
    end.

add_one(ANumber) ->
    call(plus_oner, ANumber).

call(Process, Message) ->
    Ref = make_ref(),
    Process ! {self(), Ref, Message},
    receive
        {Ref, Result} ->
            Result
    after 250 ->
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
