%% @author Bryan Fink
%% @doc This is a "hello world" resource, with a "pretty"
%%      rendered interpretation of the file defining the
%%      webmachine dispatch table for this application.
-module(wmexamples_resource).
-export([init/1, to_html/2]).

-import(html, [html/2, head/2, body/2,
               linkblock/2, divblock/2,
               h1/2, p/2, pre/2, span/2,
               table/2, th/2, tr/2, td/2,
               form/2, label/2, input/2]).

-include_lib("webmachine/include/webmachine.hrl").

%%-import(html, [html/2, body/2]).

init([]) -> {ok, undefined}.

to_html(ReqData, State) ->
    {html([],
          ["\n",head([], [linkblock([{"rel", "stylesheet"},
                                     {"type", "text/css"},
                                     {"href", "index.css"}], [])]),
           "\n",body([], [h1([], "Welcome to wmexamples"),
                          dispatch_details(ReqData)])
          ]),
     ReqData, State}.

-define(DISPATCH_FILENAME, "priv/dispatch.conf").

dispatch_details(ReqData) ->
    {ok, Dispatch} = file:consult(?DISPATCH_FILENAME),
    Url = case wrq:get_qs_value("url", ReqData) of
              "http://"++Rest ->
                  case string:tokens(Rest, "/") of
                      [_|Tokens] -> [$/|string:join(Tokens, "/")];
                      _ -> "/"
                  end;
              Rest when is_list(Rest), Rest /= [] -> Rest;
              _ -> undefined
          end,
    HasMatch = if is_list(Url) ->
                       case webmachine_dispatcher:dispatch(Url, Dispatch) of
                           {_Module, _, _, _, _, _} ->
                               true;
                           {no_dispatch_match, _} ->
                               false
                       end;
                   true ->
                       not_attempted
               end,
    [form([{"action", "http://localhost:8000/"},
              {"method", "GET"}],
             p([{"id", "testline"},
                {"class", case HasMatch of
                      true -> "match";
                      false -> "nomatch";
                      not_attempted -> "none"
                  end}],
       [label([], "Test Dispatch to:"),
              input([{"name", "url"},
                     {"type", "text"},
                     {"value", if is_list(Url) -> Url;
                                  true -> []
                               end}], []),
              input([{"type", "submit"}, {"value", "Test"}], [])])),
     p([], [?DISPATCH_FILENAME, " is exposing the following paths:"]),
     lists:reverse(element(2,
        lists:foldl(fun(D, {true, Acc}) ->
                            Matches = case webmachine_dispatcher:dispatch(
                                             Url, [D]) of
                                          {_Mod, _, _, _, _, _} ->
                                              true;
                                          {no_dispatch_match, _} ->
                                              false
                                      end,
                            {not Matches,
                             [["\n",divblock([{"class", if Matches -> "match";
                                                           true -> "pass"
                                                        end}],
                                             dispatch_detail(D))]
                              |Acc]};
                       (D, {False, Acc}) ->
                            {False,
                             [["\n",divblock([], dispatch_detail(D))]|Acc]}
                    end,
                    {HasMatch, []},
                    Dispatch)))].

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
