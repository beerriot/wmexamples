%% @author Bryan Fink
%% @doc env_resource is a toy webmachine resource intended to
%%      demonstrate possible implementations of several resource
%%      functions.  The functionality it represents is inspecting and
%%      modifying OS-environment variables.
%%
%%      The development of this resource was described at:
%%      http://blog.beerriot.com
%%        /2009/04/13/simple-webmachine-proper-http-resources/
%%        /2009/04/22/simple-webmachine-put/
%%        /2009/04/23/simple-webmachine-authorization/
%%        /2009/04/24/simple-webmachine-etags/
%%        /2009/04/27/simple-webmachine-delete/
%%
%%      GET - see resource_exists/2 and to_json/2
%%        /_env/
%%          should return a JSON structure with environment variable
%%          names as keys and their values as values
%%        /_env/KEY
%%          should return a JSON string with the value of the
%%          environment variable named KEY
%%
%%      PUT - see from_json/2
%%        /_env/   body={"KEY1":"VALUE1","KEY2":"VALUE2"}
%%          should set the value of environment variable KEY1 to VALUE1
%%          and the value of KEY2 to VALUE2
%%        /_env/KEY   body="VALUE"
%%          should set the value of environment variable KEY to VALUE
%%
%%      DELETE - see delete_resource/2
%%        /_env/KEY
%%          should "delete" KEY, such that GET /_env/ will not have
%%          KEY as a key in its response, and GET /_env/KEY will
%%          return 404 (actual implementation is that the value of
%%          environment variable KEY is set to the empty list)
%%
%%      PUT and DELETE method require a proper Authorization header,
%%      using username "webmachine" and password "rules"
%%      (see is_authorized/2)
%%
%%      Conditional requests may be performed by adding If-Match or
%%      If-None-Match headers.  Etag headers with the values to use
%%      in If-* headers are produced by generate_etag/2
-module(env_resource).
-export([init/1, content_types_provided/2, resource_exists/2, to_json/2]).
-export([allowed_methods/2, content_types_accepted/2, from_json/2]).
-export([is_authorized/2]).
-export([generate_etag/2]).
-export([delete_resource/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(_) -> {ok, undefined}.

content_types_provided(RD, Ctx) ->
    {[{"application/json", to_json}], RD, Ctx}.

resource_exists(RD, Ctx) ->
    case wrq:path_info(env, RD) of
        undefined ->
            Result = [ {K, string:join(V, "=")}
                       || [K|V] <- [ string:tokens(E, "=")
                                     || E <- os:getenv() ],
                          V /= [] ],
            {true, RD, {struct, Result}};
        Env ->
            case os:getenv(Env) of
                false  -> {false, RD, Ctx};
                []     -> {false, RD, Ctx};
                Result -> {true, RD, Result}
            end
    end.

to_json(RD, Result) ->
    {mochijson:encode(Result), RD, Result}.

%% PUT support

allowed_methods(RD, Ctx) ->
    {['GET', 'HEAD', 'PUT'
      |case wrq:path_info(env, RD) of
           undefined -> [];
           _         -> ['DELETE']
       end],
     RD, Ctx}.

content_types_accepted(RD, Ctx) ->
    {[{"application/json", from_json}], RD, Ctx}.

from_json(RD, Ctx) ->
    case wrq:path_info(env, RD) of
        undefined ->
            {struct, MJ} = mochijson:decode(wrq:req_body(RD)),
            [ os:putenv(K, V) || {K, V} <- MJ ];
        Env ->
            MJ = mochijson:decode(wrq:req_body(RD)),
            os:putenv(Env, MJ)
    end,
    {true, RD, Ctx}.

%% AUTH support

-define(AUTH_HEAD, "Basic realm=MyOSEnv").

is_authorized(RD, Ctx) ->
    case wrq:method(RD) of
        PD when PD == 'PUT'; PD == 'DELETE' -> basic_auth(RD, Ctx);
        _                                   -> {true, RD, Ctx}
    end.

basic_auth(RD, Ctx) ->
    case wrq:get_req_header("Authorization", RD) of
        "Basic "++Base64 ->
            case string:tokens(base64:mime_decode_to_string(Base64), ":") of
                ["webmachine", "rules"] -> {true, RD, Ctx};
                _                       -> {?AUTH_HEAD, RD, Ctx}
            end;
        _ -> {?AUTH_HEAD, RD, Ctx}
    end.

%% ETAG support

generate_etag(RD, Result) ->
    {mochihex:to_hex(erlang:phash2(Result)), RD, Result}.

%% DELETE support

delete_resource(RD, Ctx) ->
    os:putenv(wrq:path_info(env, RD), []),
    {true, RD, Ctx}.
