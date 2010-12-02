%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc layer3 startup code

-module(hmhj_layer3).
-author('author <author@example.com>').
-export([start/0, start_link/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.

%% @spec start_link() -> {ok,Pid::pid()}
%% @doc Starts the app for inclusion in a supervisor tree
start_link() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(hmhj_layer2),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    hmhj_layer3_sup:start_link().

%% @spec start() -> ok
%% @doc Start the layer3 server.
start() ->
    ensure_started(crypto),
    ensure_started(mochiweb),
    ensure_started(hmhj_layer2),
    application:set_env(webmachine, webmachine_logger_module, 
                        webmachine_logger),
    ensure_started(webmachine),
    application:start(hmhj_layer3).

%% @spec stop() -> ok
%% @doc Stop the layer3 server.
stop() ->
    Res = application:stop(hmhj_layer3),
    application:stop(webmachine),
    application:stop(mochiweb),
    application:stop(crypto),
    application:stop(hmhj_layer2),
    Res.
