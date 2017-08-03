-module(bitcoin_reg_db).
-behaviour(gen_server).
-include("bitcoin_reg.hrl").

%% API.
-export ([start_link/0]).
-export ([save/1]).
-export ([save/2]).
-export ([delete/1]).
-export ([match/1]).
-export ([export_csv/0]).

%% gen_server.
-export([init/1]).
-export([handle_call/3]).
-export([handle_cast/2]).
-export([handle_info/2]).
-export([terminate/2]).
-export([code_change/3]).

-record(state, {
  ref   %% DB ref
}).

-define(SERVER, ?MODULE).
% -define(record_to_tuplelist(Rec, Ref), lists:zip(record_info(fields, Rec),tl(tuple_to_list(Ref)))).
% -define(map_to_record(Rec, Ref), list_to_tuple([Rec|maps:to_list(Ref)])).

-spec start_link() -> {ok, pid()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

save(Data) ->
  gen_server:call(?SERVER, {save, Data}).

save(Data, Validators) ->
  gen_server:call(?SERVER, {save, Data, Validators}).

delete(Data) ->
  gen_server:call(?SERVER, {delete, Data}).

match(Qualifier) ->
  gen_server:call(?SERVER, {match, Qualifier}).

export_csv() ->
  gen_server:call(?SERVER, {export, csv}).

%% ------------------------------------------------------------------
%% gen_server callbacks
%% ------------------------------------------------------------------
init([]) ->
  ?INFO("Module ~p started on node ~p~n", [?SERVER, node()]),
  DBs = [
    {btc_user, record_info(fields, btc_user)},
    {btc_reg, record_info(fields, btc_reg)}
  ],
  setup_db(DBs),
  {ok, #state{}}.

handle_call({save, Data}, _From, State) ->
  Reply = mnesia:dirty_write(Data),
  ?INFO("Saving user data validator: ~p~n", [Reply]),
  {reply, {ok, Reply}, State};

handle_call({save, Data, Validators}, _From, State) ->
  Reply = 
    case validate_data(Validators) of
      ok ->
        mnesia:dirty_write(Data);
      Err ->
        Err
    end,
  ?INFO("Saving user data validator: ~p~n", [Reply]),
  {reply, Reply, State};

handle_call({delete, Data}, _From, State) ->
  Reply = mnesia:dirty_delete_object(Data),
  ?INFO("Deleting data: ~p~n", [Reply]),
  {reply, {ok, Reply}, State};

handle_call({match, Qualifier}, _From, State) ->
  ?DEBUG("Qualifier = ~p~n", [Qualifier]),
  case mnesia:dirty_match_object(Qualifier) of 
    [] -> {reply, {error, enoent}, State};
    Data -> {reply, {ok, Data}, State}
  end;

handle_call({export, csv}, _From, State) ->
  Resp = mnesia:dirty_match_object(#btc_reg{_='_'}),
  {reply, {ok, Resp}, State};

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


%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
setup_db(Databases) ->
  setup_db(Databases, false).

setup_db(Databases, Reset) ->
  application:stop(mnesia),
  case Reset of
    true ->
      mnesia:delete_schema([node()|nodes()]);
    _ ->
      ok
  end,
  mnesia:create_schema([node()|nodes()]),
  ok = application:start(mnesia),
  mnesia_eleveldb:register(),
  create(Databases).

create([{Table, Fields}|Rest]) ->
  create_table(Table, Fields),
  create(Rest);
create([]) -> ok.

create_table(Table, Fields) ->
  mnesia:create_table(Table, [
    {attributes, Fields},
    {leveldb_copies, [node()]},
    {type, ordered_set}]),
  mnesia:wait_for_tables([Table], 1000).

validate_data([H|T]) ->
  ?DEBUG("Validating rule: ~p~n", [H]),
  case mnesia:dirty_match_object(H) of
    [] -> validate_data(T);
    _  -> {error, dups}
  end;
validate_data([]) -> ok.

