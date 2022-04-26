%% @hidden
-module(nuntius_expect).

-export([delete/2]).
-export([list/1]).
-export([save/3]).

%% @doc Adds a new expect function to a mocked process.
-spec save(nuntius:process_name(), nuntius:expect_id(), nuntius:expect_fun()) -> ok.
save(ProcessName, ExpectId, Function) ->
    nuntius_mocker:cast(ProcessName, {expect, Function, ExpectId}).

%% @doc Removes an expect function.
-spec delete(nuntius:process_name(), nuntius:expect_id()) -> ok.
delete(ProcessName, ExpectId) ->
    nuntius_mocker:cast(ProcessName, {delete, ExpectId}).

%% @doc Returns the list of expect functions for a process.
-spec list(nuntius:process_name()) -> #{nuntius:expect_id() => nuntius:expect_fun()}.
list(ProcessName) ->
    nuntius_mocker:call(ProcessName, expects).
