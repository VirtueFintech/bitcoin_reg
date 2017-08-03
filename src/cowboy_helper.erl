-module (cowboy_helper).
-include ("bitcoin_reg.hrl").

-export ([method/1]).
-export ([path/1]).
-export ([method_and_path/1]).
-export ([binding/2]).
-export ([query/1]).
-export ([data/1]).
-export ([json_data/1]).
-export ([render/4]).
-export ([status/2]).
-export ([json_reply/2]).
-export ([json_reply/3]).
-export ([json_response/5]).
-export ([redirect/3]).

-type req()     :: cowboy_req:req().
-type path()    :: binary().
-type method()  :: binary().
-type addr()    :: binary().
-type page()    :: atom().
-type params()  :: list().
-type json()    :: binary().

-spec method(Req::req())
      -> {ok, method(), req()}.
method(Req) ->
  {Method, Req1} = cowboy_req:method(Req),
  {ok, Method, Req1}.

-spec path(Req::req())
      -> {ok, path(), req()}.
path(Req) ->
  {Path, Req1} = cowboy_req:path(Req),
  {ok, Path, Req1}.

-spec method_and_path(Req::req())
      -> {ok, method(), path(), req()}.
method_and_path(Req) ->
  {Method, Req1} = cowboy_req:method(Req),
  {Path, Req2} = cowboy_req:path(Req1),
  {ok, Method, Path, Req2}.

-spec binding(Bind::atom(), Req::req())
      -> {ok, addr(), req()}.
binding(Bind, Req) ->
  {Address, Req1} = cowboy_req:binding(Bind, Req),
  {ok, Address, Req1}.

-spec query(Req::req())
      -> {ok, map(), req()}.
query(Req) ->
  {QS, Req2} = cowboy_req:qs_vals(Req),
  {ok, maps:from_list(QS), Req2}.

-spec data(Req::req())
      -> {ok, map(), req()}.
data(Req) ->
  {ok, Data, Req1} = cowboy_req:body_qs(Req),
  {ok, maps:from_list(Data), Req1}.

-spec json_data(Req::req())
      -> {ok, map(), req()}.
json_data(Req) ->
  case cowboy_req:body_qs(Req) of
    {ok, [], Req1} ->
      {ok, #{}, Req1};
    {ok, [{Json, true}], Req1} ->
      Data = jsx:decode(Json, [return_maps]),
      {ok, Data, Req1};
    {ok, [{A, B}], Req1} ->
      Val = <<A/binary, B/binary>>,
      Data = jsx:decode(Val, [return_maps]),
      {ok, Data, Req1}
  end.

-spec render(Template::page(), Params::params(), Req::req(), State::term())
      -> {ok, req(), term()}.
render(Template, Params, Req, State) ->
  % Page2 = binary_to_existing_atom(<<Page/binary, "_dtl">>, utf8),
  {ok, Content} = apply(Template, render, [Params]),
  Req1 = cowboy_req:set_resp_body(Content, Req),
  {ok, Req2} = cowboy_req:reply(200, Req1),
  {ok, Req2, State}.

-spec status(Status::integer(), Req::req())
      -> {ok, req()}.
status(Status, Req) ->
  {ok, Req1} = cowboy_req:reply(Status, Req),
  {ok, Req1}.

-spec json_reply(Reply::list(), Req::req())
      -> {ok, req()}
      |  {error, term(), req()}.
json_reply(Reply, Req) ->
  try
    Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
    Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
    {ok, Req2}
  catch
    _:Err ->
      {error, Err, Req}
  end.

-spec json_reply(Status::integer(), Reply::list(), Req::req())
      -> {ok, req()}
      |  {error, term(), req()}.
json_reply(Status, Reply, Req) ->
  try
    Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
    Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
    {ok, Req3} = cowboy_req:reply(Status, Req2),
    {ok, Req3}
  catch
    _:Err ->
      {error, Err, Req}
  end.

-spec redirect(Location::path(), Req::req(), State::term())
      -> {ok, req(), term()}.
redirect(Location, Req, State) ->
  {ok, Req2} = cowboy_req:reply(303, [{<<"location">>, Location}], Req),
  {ok, Req2, State}.


-spec json_response(
        Type :: get | post,
        Status :: success | error,
        Data :: term(),
        Req :: req(),
        State :: term())
      -> {json(), req(), term()}
      |  {true, req(), term()}
      |  {false, req(), term()}.
json_response(Type, Status, Data, Req, State) ->
  case Type of
    get ->
      case Status of
        success ->
          Reply = [{status, success}, {data, Data}],
          {jsx:encode(Reply), Req, State};
        error ->
          Reply = [{status, error}, {error_message, [{error, Data}]}],
          {jsx:encode(Reply), Req, State}
      end;

    post ->
      case Status of
        success ->
          Reply = [{status, success}, {data, Data}],
          {ok, Req1} = cowboy_helper:json_reply(Reply, Req),
          {true, Req1, State};
        error ->
          Reply = [{status, error}, {data, Data}],
          {ok, Req1} = cowboy_helper:json_reply(Reply, Req),
          {false, Req1, State}
      end
  end.
