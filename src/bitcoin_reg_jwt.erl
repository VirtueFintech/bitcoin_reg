-module(bitcoin_reg_jwt).
-behaviour(gen_server).

%% API.
-export ([start_link/0]).
-export ([encode/1]).
-export ([decode/1]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {  
  secret  %% shared secret key
}).

-define(SERVER, ?MODULE).
-define(JWT_SHARED_KEY, <<"Th3M4nWh0Kn0wsInfin1ty">>).

%% API.
encode(Data) ->
  gen_server:call(?SERVER, {encode, Data}).

decode(Data) ->
  gen_server:call(?SERVER, {decode, Data}).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% gen_server.

init([]) ->
  SharedSecret = 
    case os:getenv(<<"JWT_SHARED_KEY">>, none) of
      none -> ?JWT_SHARED_KEY;
      Key -> Key
    end,
  {ok, #state{secret=SharedSecret}}.

handle_call({encode, Data}, _From, State = #state{secret=SharedSecret}) ->
  Encoded = ejwt:encode(Data, SharedSecret),
  {reply, {ok, Encoded}, State};

handle_call({decode, Data}, _From, State = #state{secret=SharedSecret}) ->
  Decoded = ejwt:decode(Data, SharedSecret),
  {reply, {ok, Decoded}, State};

handle_call(_Request, _From, State) ->
  {reply, ignored, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

-ifdef(TEST).

-include_lib("eunit/include/eunit.hrl").

encode_decode_test() ->
  Secret = "FooIsBar",
  Data = [{<<"username">>, <<"hisham">>}],
  Enc = ejwt:encode(Data, Secret),
  Dec = ejwt:decode(Enc, Secret),
  Data = Dec.

-endif.
