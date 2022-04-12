%%% @doc Tests for the top-level API.
-module(nuntius_api_SUITE).

-behaviour(ct_suite).

-export([all/0, init_per_suite/1, end_per_suite/1, init_per_testcase/2,
         end_per_testcase/2]).
-export([default_mock/1, error_not_found/1, delete_mock/1, mocked_processes/1]).

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
