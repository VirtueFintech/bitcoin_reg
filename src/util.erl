-module (util).

-export ([json/4]).
-export ([uuid/0]).
-export ([hash/1]).

-type req() :: cowboy_req:req().
-type state() :: iodata().

-spec json(Method :: atom(),
           Reply :: iodata(), 
           Req :: req(),
           State :: state()) -> {true, req(), state()}.
json(get, Reply, Req, State) ->
  {jsx:encode(Reply), Req, State};

json(_, Reply, Req, State) ->
  Req1 = cowboy_req:set_resp_header(<<"content-type">>, <<"application/json">>, Req),
  Req2 = cowboy_req:set_resp_body(jsx:encode(Reply), Req1),
  Req3 = cowboy_req:reply(200, Req2),
  {true, Req3, State}.

-spec uuid() -> binary().
uuid() ->
  list_to_binary(uuid:uuid_to_string(uuid:get_v4())).

-spec hash(Data::binary()) -> binary().
hash(Data) ->
  Bin = crypto:hash(sha256, crypto:hash(sha256, Data)),
  base58:encode(Bin).
