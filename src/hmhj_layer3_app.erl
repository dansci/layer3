%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the layer3 application.

-module(hmhj_layer3_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for layer3.
start(_Type, _StartArgs) ->
    hmhj_layer3_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for layer3.
stop(_State) ->
    ok.
