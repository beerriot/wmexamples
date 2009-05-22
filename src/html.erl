%% @author Bryan Fink
%% @doc Simple functions for generating HTML.  This first appeared
%%      in webmachine's wmtrace utility.  I'm extracting it to its
%%      own module for easy reuse.
%%      Example:
%%        html([],
%%             [body([],
%%                   [h1([], ["Hello, World!"]),
%%                    p([{"style", "font-type:italic"}],
%%                      "That's easy")
%%                   ])
%%             ])
%%      Should produce:
%%        <html><body>
%%        <h1>Hello, World!</h1>
%%        <p style="font-type:italic">That's easy</p>
%%        </body></html>
-module(html).
-compile(export_all).

-define(TAG(T), T(Attrs, Content) ->
                   tag(??T, Attrs, Content)).

?TAG(head).
?TAG(script).
?TAG(title).
?TAG(body).
?TAG(h1).
?TAG(h2).
?TAG(h3).
?TAG(ul).
?TAG(li).
?TAG(a).
?TAG(p).
?TAG(canvas).
?TAG(select).
?TAG(pre).
?TAG(span).
?TAG(button).
?TAG(em).
?TAG(strong).
?TAG(table).
?TAG(th).
?TAG(tr).
?TAG(td).

html(_Attrs, Content) ->
    [<<"<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">\n">>,
     <<"<html xmlns=\"http://www.w3.org/1999/xhtml\" xml:lang=\"en\" lang=\"en\">">>,
     Content,
     <<"</html>">>].

divblock(Attrs, Content) ->
    tag("div", Attrs, Content). %% div is a reserved word

linkblock(Attrs, Content) ->
    tag("link", Attrs, Content). %% link is a reserved word

tag(Name, Attrs, Content) ->
    ["<",Name,
     [ [" ",K,"=\"",V,"\""] || {K, V} <- Attrs ],
     if Content == empty -> "/>";
        true ->
             [">",
              Content,
              "</",Name,">"]
     end].
