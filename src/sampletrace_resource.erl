%% @author Bryan Fink
%% @doc sampletrace_resource is intended to do nothing more than
%%      generate a few sample trace files for experimenting with the
%%      wmtrace utility.  As such, it has a fairly strange structure,
%%      forcing itself through a few resource functions to make
%%      webmachine generate requested status codes.
%%
%%      Make requests to this resource specifying a code with the
%%      query parameter ?code=XXX, where XXX is one of
%%      200, 301, 404, or 500
%%
%%      Hint: You can also generate a sample 400-code trace by
%%      asking for an unsupported code.
-module(sampletrace_resource).
-export([init/1,
         malformed_request/2,
         resource_exists/2,
         previously_existed/2,
         moved_permanently/2,
         to_html/2]).
-include_lib("webmachine/include/webmachine.hrl").

init(_) ->
    {{trace, "traces"}, %% enable tracing, and store files
                        %% in "traces" directory
     undefined}.

malformed_request(RD, undefined) ->
    case wrq:get_qs_value("code", RD) of
        "500"     -> {false, RD, sample500};
        "404"     -> {false, RD, sample404};
        "301"     -> {false, RD, sample301};
        "200"     -> {false, RD, sample200};
        undefined -> {false, RD, sample200};
        Other ->
            {true,
             wrq:append_to_response_body(
               ["Status Code ", Other, " is not supported."],
               wrq:set_resp_header("Content-type",
                                   "text/html",
                                   RD)),
             undefined}
    end.

resource_exists(RD, sample200)  -> {true,  RD, sample200};
resource_exists(RD, Code)       -> {false, RD, Code}.

previously_existed(RD, sample301)  -> {true,  RD, sample301};
previously_existed(RD, sample404)  -> {false, RD, sample404}.

moved_permanently(RD, sample301)  -> {{true, "/"}, RD, sample301}.

to_html(RD, sample200) ->
    {"<html><body>Hello!</body></html>", RD, sample200}.
