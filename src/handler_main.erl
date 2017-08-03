-module(handler_main).

-export([init/3]).
-export([content_types_provided/2]).
-export([get_html/2]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

content_types_provided(Req, State) ->
  {[{{<<"text">>, <<"html">>, '*'}, get_html}], Req, State}.

get_html(Req, State) ->
  {<<"<html><body>Invalid request</body></html>">>, Req, State}.
