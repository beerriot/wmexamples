%% @author author <author@example.com>
%% @copyright YYYY author.

%% @doc TEMPLATE.

-module(wmexamples).
-author('author <author@example.com>').
-export([start/0, stop/0]).

ensure_started(App) ->
    case application:start(App) of
	ok ->
	    ok;
	{error, {already_started, App}} ->
	    ok
    end.
	
%% @spec start() -> ok
%% @doc Start the wmexamples server.
start() ->
    wmexamples_deps:ensure(),
    ensure_started(crypto),
    ensure_started(webmachine),
    application:start(wmexamples).

%% @spec stop() -> ok
%% @doc Stop the wmexamples server.
stop() ->
    Res = application:stop(wmexamples),
    application:stop(webmachine),
    application:stop(crypto),
    Res.
