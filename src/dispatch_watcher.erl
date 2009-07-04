%% @author Bryan Fink
%% @doc dispatch_watcher monitors a file that is expected to contain
%%      a Webmachine dispatcher configuration.  The file will be
%%      reloaded when its mtime changes.

-module(dispatch_watcher).
-include_lib("kernel/include/file.hrl").

-behaviour(gen_server).
-export([start/0, start/1,
         start_link/0, start_link/1,
         stop/0,
         force/0,
         init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {last, dispatch, interval}).

-define(DEFAULT_DISPATCH, "priv/dispatch.conf").
-define(DEFAULT_INTERVAL, 1000).

%% External API

%% @spec start() -> Result
%% @doc Start the watcher with the default dispatch path
%%      and monitor interval.
start() ->
    start([]).

%% @spec start(Options) -> Result
%% @type Options = [Option]
%% @type Option = {dispatch, Path}|{interval, integer()}
%% @type Path = string()
%% @doc Start the watching, monitoring the file given by the
%%      'dispatch' path option at the interval given by the
%%      'interval' option, in milliseconds (or the default path
%%      and interval if the option is not specified).
start(Props) ->
    gen_server:start({local, ?MODULE}, ?MODULE, Props, []).

%% @spec start_link() -> Result
%% @see start/0
start_link() ->
    start_link([]).

%% @spec start_link(Options) -> Result
%% @see start/1
start_link(Props) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Props, []).

stop() ->
    gen_server:call(?MODULE, stop).

%% @spec force() -> ok|Error
%% @doc Force an immediate attempt to reload the dispatch file.
force() ->
    gen_server:call(?MODULE, reload_now).

%% gen_server callbacks

init(Props) ->
    Dispatch = proplists:get_value(dispatch, Props,
                                   ?DEFAULT_DISPATCH),
    Interval = proplists:get_value(interval, Props,
                                  ?DEFAULT_INTERVAL),
    {ok, #state{last=stamp(),
                dispatch=Dispatch,
                interval=Interval},
     Interval}.

handle_call(reload_now, _From, State) ->
    {Outcome, NewState} = attempt_reload(State, true),
    {reply, Outcome, NewState, NewState#state.interval};
handle_call(stop, _From, State) ->
    {stop, shutdown, stopped, State};
handle_call(_Req, _From, State) ->
    {reply, {error, badrequest}, State, State#state.interval}.

handle_cast(_Req, State) ->
    {noreply, State, State#state.interval}.

handle_info(timeout, State) ->
    {_Outcome, NewState} = attempt_reload(State, false),
    {noreply, NewState, NewState#state.interval};
handle_info(_Info, State) ->
    {noreply, State, State#state.interval}.

terminate(_Reason, _State) ->
    ok.

code_change(_Vsn, State, _Extra) ->
    {ok, State}.

%% Internal API

attempt_reload(State, Force) ->
    Now = stamp(),
    Changed = case file:read_file_info(State#state.dispatch) of
                  {ok, FileInfo} ->
                      FileInfo#file_info.mtime >= State#state.last
                          andalso FileInfo#file_info.mtime < Now;
                  {error, Reason} ->
                      {error, Reason}
              end,
    case {Changed, Force} of
        {true, _}      -> attempt_reload2(State#state{last=Now});
        {false, true}  -> attempt_reload2(State#state{last=Now});
%%% do not update timestamp if reload not attempted
        {false, false} -> {nothing_to_do, State};
        {Error, _}     ->
            io:format("Dispatch reload aborted: ~p~n", [Error]),
            {Error, State}
    end.

attempt_reload2(State) ->
    case file:consult(State#state.dispatch) of
        {ok, NewDispatch} ->
            application:set_env(webmachine, dispatch_list, NewDispatch),
            io:format("Reloaded dispatch from ~p~n", [State#state.dispatch]),
            {ok, State};
        Error ->
            io:format("Dispatch reload failed: ~p~n", [Error]),
            {Error, State}
    end.

stamp() ->
    erlang:localtime().
