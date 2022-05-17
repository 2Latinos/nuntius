%% @hidden
-module(nuntius_history).

-export([get/1]).
-export([received/2]).
-export([reset/1]).

%% @doc Returns the history of messages received by a mocked process.
-spec get(nuntius:process_name()) -> [nuntius:event()].
get(ProcessName) ->
    nuntius_mocker:call(ProcessName, history).

%% @doc Returns whether a particular message was received already.
-spec received(nuntius:process_name(), term()) -> boolean().
received(ProcessName, Message) ->
    nuntius_mocker:call(ProcessName, {received, Message}).

%% @doc Erases the history for a mocked process.
%%      Note that there is no gen:cast(...),
%%      gen_server and others basically just send the message and move on, like us.
-spec reset(nuntius:process_name()) -> ok.
reset(ProcessName) ->
    nuntius_mocker:cast(ProcessName, reset_history).
