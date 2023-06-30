%% @hidden
-module(nuntius_proc).

-export([current_message/0, current_message/1]).
-export([passed_through/0, passed_through/1]).
-export([passthrough/0, passthrough/1]).
-export([pid/0, pid/1]).

%% @doc Passes the current message down to the mocked process.
-spec passthrough() -> ok.
passthrough() ->
    passthrough(current_message()).

%% @doc Passes a message down to the mocked process.
-spec passthrough(term()) -> ok.
passthrough(Message) ->
    ProcessPid = pid(),
    passed_through(true),
    ProcessPid ! Message,
    ok.

%% @doc Returns the PID of the currently mocked process.
-spec pid() -> pid().
pid() ->
    get(process_pid).

pid(ProcessPid) ->
    put(process_pid, ProcessPid).

current_message() ->
    get(current_message).

current_message(Message) ->
    put(current_message, Message).

passed_through(Flag) ->
    put(passed_through, Flag).

passed_through() ->
    get(passed_through).
