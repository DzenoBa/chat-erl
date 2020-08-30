%%%-------------------------------------------------------------------
%% @doc chat_client public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    websocket_handler:start_link(),
    gui:start_link(),
    chat_client_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
