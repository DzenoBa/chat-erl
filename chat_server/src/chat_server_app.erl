%%%-------------------------------------------------------------------
%% @doc chat_server public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_server_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Routes = [{"/", cowboy_hello_world, []}, {"/chat", cowboy_websocket_handler, []}],
    Dispatch = cowboy_router:compile([{'_', Routes}]),
    {ok, _} = cowboy:start_clear(http, [{port, 8080}], #{env => #{dispatch => Dispatch}}),
    pg:start_link(),
    chat_server_sup:start_link().

stop(_State) ->
    cowboy:stop_listener(http),
    ok.
