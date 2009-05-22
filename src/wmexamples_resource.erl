%% @author Bryan Fink
%% @doc This is a "hello world" resource, with a "pretty"
%%      rendered interpretation of the file defining the
%%      webmachine dispatch table for this application.
-module(wmexamples_resource).
-export([init/1, to_html/2]).

-import(html, [html/2, head/2, body/2,
               linkblock/2,
               h1/2, p/2, pre/2, span/2,
               table/2, th/2, tr/2, td/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%-import(html, [html/2, body/2]).

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {html([],
          ["\n",head([], [linkblock([{"rel", "stylesheet"},
                                     {"type", "text/css"},
                                     {"href", "index.css"}], [])]),
           "\n",body([], [h1([], "Welcome to wmexamples"),
                          dispatch_details()])
          ]),
     ReqData, State}.

-define(DISPATCH_FILENAME, "priv/dispatch.conf").

dispatch_details() ->
    {ok, Dispatch} = file:consult(?DISPATCH_FILENAME),
    [p([], [?DISPATCH_FILENAME, " is exposing the following paths:"]),
     [["\n",dispatch_detail(D)] || D <- Dispatch]].

dispatch_detail({Path, Resource, Args}) ->
    table([],
          [tr([], th([{"colspan", "2"}], dispatch_path(Path))), "\n",
           tr([],
              [td([{"class", "label"}], "resource"),
               td([], dispatch_resource(Resource))]), "\n",
           tr([],
              [td([{"class", "label"}], "argument"),
               td([], dispatch_args(Args))])
          ]).

dispatch_path([]) ->
    "/";
dispatch_path(Path) ->
    [["/", dispatch_path_part(P)] || P <- Path].

dispatch_path_part(Literal) when is_list(Literal) ->
    Literal;
dispatch_path_part('*') ->
    "&hellip;";
dispatch_path_part(Match) when is_atom(Match) ->
    span([{"class", "pathmatch"}], atom_to_list(Match)).


dispatch_resource(Resource) ->
    atom_to_list(Resource).

dispatch_args(Args) ->
    pre([], io_lib:format("~p", [Args])).
