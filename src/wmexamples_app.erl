%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc Callbacks for the wmexamples application.

-module(wmexamples_app).
-author('author <author@example.com>').

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for wmexamples.
start(_Type, _StartArgs) ->
    wmexamples_deps:ensure(),
    wmexamples_sup:start_link().

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for wmexamples.
stop(_State) ->
    ok.
