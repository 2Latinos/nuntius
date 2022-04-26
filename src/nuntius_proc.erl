%% @hidden
-module(nuntius_proc).

-export([current_message/0, current_message/1]).
-export([passed_through/0, passed_through/1]).
-export([passthrough/0, passthrough/1]).
-export([process/0, process/1]).

%% @doc Passes the current message down to the mocked process.
-spec passthrough() -> ok.
passthrough() ->
    passthrough(current_message()).

%% @doc Passes a message down to the mocked process.
-spec passthrough(term()) -> ok.
passthrough(Message) ->
    ProcessPid = process(),
    passed_through(true),
    ProcessPid ! Message.

%% @doc Returns the PID of the currently mocked process.
-spec process() -> pid().
process() ->
    get(process_pid).

process(ProcessPid) ->
    put(process_pid, ProcessPid).

current_message() ->
    get(current_message).

current_message(Message) ->
    put(current_message, Message).

passed_through(Flag) ->
    put(passed_through, Flag).

passed_through() ->
    get(passed_through).
