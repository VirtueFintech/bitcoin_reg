-module(handler_btcreg).
-include ("bitcoin_reg.hrl").

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([is_authorized/2]).
-export([route/2]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.  

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, route}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, route}], Req, State}.

resource_exists(Req, State) ->
  {true, Req, State}.

is_authorized(Req, State) ->
  case cowboy_req:parse_header(<<"x-auth">>, Req) of 
    {_, undefined, Req1} ->
      {true, Req1, State};
    {_, Token, Req1} ->
      ?INFO("Token: ~p~n", [Token]),
      case bitcoin_reg_jwt:decode(Token) of
        {ok, JWT} ->
          ?DEBUG("Authentication is ok!~n"),
          {true, Req1, JWT};
        _ ->
          ?ERROR("Invalid authentication...~n"),
          {true, Req1, State}
      end
  end.

route(Req, State) ->
  {ok, Method, Path, Req1} = cowboy_helper:method_and_path(Req),
  ?DEBUG("~p: Processing ~p, ~p~n", [?MODULE, Method, Path]),
  route(Method, Path, Req1, State).

route(<<"POST">>, <<"/api/v1/register">>, Req, State) ->
  {ok, Data, Req1} = cowboy_helper:json_data(Req),
  ?INFO("Processing bitcoin registration: ~p~n", [Data]),

  BTCAddress = maps:get(<<"btc_address">>, Data, <<"">>),
  BTCTxHash = maps:get(<<"btc_tx_hash">>, Data, <<"">>),
  BTCSig = maps:get(<<"btc_sig">>, Data, <<"">>),
  ETHAddress = maps:get(<<"eth_address">>, Data, <<"">>),
  Contact = maps:get(<<"contact">>, Data, <<"">>),
  Referrer = maps:get(<<"referrer">>, Data, <<"">>),

  ?INFO("Submitted data: ~p, ~p, ~p, ~p, ~p, ~p~n", 
    [BTCAddress, BTCTxHash, BTCSig, ETHAddress, Contact, Referrer]),

  % check for all mandatory fields
  case BTCAddress =:= <<"">> orelse
       BTCTxHash =:= <<"">> orelse
       BTCSig =:= <<"">> orelse
       ETHAddress =:= <<"">> orelse
       Contact =:= <<"">> of
    true ->
      Res = [
        {status, error}, 
        {message, <<"Parameters btc_address, btc_tx_hash, eth_address, btc_sig and contact are mandatory">>}
      ],
      {ok, Req2} = cowboy_helper:json_reply(Res, Req1),
      {false, Req2, State};
    false ->
      % ok, see if we can decode the signature
      Sig = <<BTCSig/binary, "=">>,
      Foo = base64:decode(Sig),
      case size(Foo) < 120 of
        true ->
          Res = [
            {status, error}, 
            {message, <<"BTC Signature is not encoded in base64!">>}
          ],
          {ok, Req4} = cowboy_helper:json_reply(Res, Req1),
          {false, Req4, State};
        false ->
          {ok, TS} = tempo:format(iso8601, {now, erlang:timestamp()}),
          UUID = util:uuid(),
          User = #btc_reg{uuid=UUID, btc_address=BTCAddress, btc_tx_hash=BTCTxHash,
                    eth_address=ETHAddress, btc_sig=Sig, contact=Contact, referrer=Referrer,
                    created_at=TS, updated_at=TS},

          ?INFO("Saving registration data: ~p~n", [User]),
          case bitcoin_reg_db:save(User) of
            {error, Err} ->
              {ok, Req3} = cowboy_helper:json_reply([{error, Err}], Req1),
              {false, Req3, State};
            _ ->
              Resp = [{ok, <<"data added">>}],
              {ok, Req3} = cowboy_helper:json_reply(Resp, Req1),
              {true, Req3, State}
          end
      end
  end;

route(<<"POST">>, <<"/api/v1/register/download">>, Req, State) ->
  ?INFO("Downloading request. State = ~p~n", [State]),
  case State =:= undefined of
    true ->
      Resp = [{error, <<"authentcation required">>}],
      {ok, Req1} = cowboy_helper:json_reply(Resp, Req),
      {true, Req1, State};
    false ->
      {ok, All} = bitcoin_reg_db:match(#btc_reg{_='_'}),
      Resp = [{status, ok}, {data, parse_all(All)}],
      {ok, Req1} = cowboy_helper:json_reply(Resp, Req),
      {false, Req1, State}
  end;

route(<<"GET">>, Path, Req, State) ->
  ?WARN("Invalid request: GET -> ~p~n", [Path]),
  {jsx:encode([{error, enoimpl}]), Req, State};

route(Method, Path, Req, State) ->
  ?WARN("Invalid request: ~p -> ~p~n", [Method, Path]),
  Resp = [{error, enoimpl}],
  {ok, Req1} = cowboy_helper:json_reply(Resp, Req),
  {false, Req1, State}.

%% =============================================================================
%% private functions
%% =============================================================================
parse_all(Data) ->
  parse_all(Data, []).

parse_all([H|T], Accu) ->
  %% parse the sig, and add =
  N = H#btc_reg.btc_sig,
  Sig = H#btc_reg{btc_sig= base64:decode(N)},

  Rec = ?record_to_tuplelist(btc_reg, Sig),
  ?INFO("Rec: ~p~n", [Rec]),
  parse_all(T, [Rec|Accu]);

parse_all([], Accu) ->
  lists:reverse(Accu).
