%% @author Bryan Fink
%% @doc formjson_resource takes x-www-form-urlencoded data and
%%      re-encodes it as JSON.  The data is expected as query
%%      parameters for a GET, and request body for a POST.
%%
%%      In:  one=two&me=pope
%%      Out: {"one":"two","me":"pope"}
-module(formjson_resource).
-export([init/1,
         allowed_methods/2,
         content_types_provided/2,
         to_json/2,
         process_post/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'POST'], RD, Ctx}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

%% hit this with
%%   curl "http://localhost:8000/formjson?one=two&me=pope"
to_json(RD, Ctx) ->
    {json_body(wrq:req_qs(RD)), RD, Ctx}.

%% hit this with
%%   curl -X POST http://localhost:8000/formjson \
%%        -d "one=two&me=pope"
process_post(RD, Ctx) ->
    Body = json_body(mochiweb_util:parse_qs(wrq:req_body(RD))),
    {true, wrq:append_to_response_body(Body, RD), Ctx}.

json_body(QS) -> mochijson:encode({struct, QS}).
