-module(handler_auth).
-include ("bitcoin_reg.hrl").

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_accepted/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).
-export([delete_resource/2]).
-export([delete_completed/2]).

-export([route/2]).

init(_, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"PUT">>, <<"DELETE">>, <<"OPTIONS">>], Req, State}.  

content_types_accepted(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, route}], Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, '*'}, route}], Req, State}.

resource_exists(Req, State) -> ?INFO("Checking resource...~n"),
  case cowboy_req:binding(username, Req) of 
    {undefined, Req1} ->
      {true, Req1, State};
    {Username, Req1} ->
      Spec = #btc_user{username= Username, _='_'},
      case bitcoin_reg_db:match(Spec) of
        {ok, [User]} -> 
          ?INFO("User: ~p~n", [User]),
          {true, Req1, {Username, User}};
        _ -> 
          {true, Req1, State}
      end
  end.

delete_resource(Req, State) ->
  % ?INFO("Deleting resource...~p~n", [cowboy_req:binding(username, Req)]),
  case cowboy_req:binding(username, Req) of
    {undefined, Req1} ->
      ?WARN("Unable to delete user~n"),
      Resp = [{status, failed}, {error, <<"User not found">>}],
      {ok, Req1} = cowboy_helper:json_reply(Resp, Req),
      {false, Req1, State};
    {Username, Req1} -> 
      Spec = #btc_user{username= Username, _='_'},
      case bitcoin_reg_db:match(Spec) of
        {ok, [User]} ->
          ?INFO("Deleting user ~p~n", [User]),
          bitcoin_reg_db:delete(User),
          Resp = [{status, ok}, {data, [{deleted, User#btc_user.uuid}]}],
          {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
          {true, Req2, State};          
        _ ->
          ?WARN("Unable to delete user~n"),
          Resp = [{status, failed}, {error, <<"User not found">>}],
          {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
          {false, Req2, State}
      end;
    _ ->
      ?WARN("Unable to delete user~n"),
      Resp = [{status, failed}, {error, <<"User not found">>}],
      {ok, Req1} = cowboy_helper:json_reply(Resp, Req),
      {false, Req1, State}
  end.

delete_completed(Req, State) ->
  ?INFO("Delete completed...~n"),
  {true, Req, State}.

route(Req, State) ->
  {ok, Method, Path, Req1} = cowboy_helper:method_and_path(Req),
  ?INFO("~p: Processing ~p, ~p, ~p~n", [?MODULE, Method, Path, State]),
  route(Method, Path, Req1, State).

route(<<"GET">>, <<"/auth/", Username/binary>>, Req, State) ->
  ?INFO("Getting info for ~p~n", [Username]),
  case bitcoin_reg_db:match(#btc_user{username=Username, _='_'}) of 
    {ok, [User]} ->
      Resp = [{ok, ?record_to_tuplelist(btc_user, User)}],
      {jsx:encode(Resp), Req, State};
    _ ->
      {jsx:encode([{error, enoimpl}]), Req, State}
  end;

route(<<"POST">>, <<"/auth">>, Req, State) ->
  ?INFO("Creating new user~n"),
  {ok, Data, Req1} = cowboy_helper:json_data(Req),
  ?INFO("UserId: ~p, Data: ~p~n", [State, Data]),

  % ok, we need username, email, password.
  Username = maps:get(<<"username">>, Data, <<"">>),
  Password = maps:get(<<"password">>, Data, <<"">>),

  case Username =:= <<"">> orelse
       Password =:= <<"">> of
    true ->
      %% ok, error here
      Resp = [{error, <<"Params username and password are mandatory">>}],
      {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
      {false, Req2, State};
    false ->
      % ok, create the user.
      {ok, TS} = tempo:format(iso8601, {now, erlang:timestamp()}),
      UUID = util:uuid(),
      User = #btc_user{uuid=UUID, username=Username, password=util:hash(Password), 
                created_at=TS, updated_at=TS},

      ?INFO("Saving user data: ~p~n", [User]),
      Validators = [
        #btc_user{username = Username, _='_'}
      ],
      case bitcoin_reg_db:save(User, Validators) of
        {error, Err} ->
          {ok, Req2} = cowboy_helper:json_reply([{error, Err}], Req1),
          {false, Req2, State};
        _ ->
          Resp = [{ok, ?record_to_tuplelist(btc_user, User)}],
          {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
          {true, Req2, State}
      end

  end;

route(<<"POST">>, <<"/auth/", Username/binary>>, Req, {Username, User} = State) ->
  ?INFO("Authenticating user ~p, State: ~p~n", [Username, State]),
  {ok, Data, Req1} = cowboy_helper:json_data(Req),
  Password = maps:get(<<"password">>, Data, <<"">>),
  Hash = util:hash(Password),

  ?INFO("Hash: ~p, Password: ~p~n", [Hash, User#btc_user.password]),
  case Hash =:= User#btc_user.password of 
    true ->
      JData = [{username, Username}],
      {ok, Token} = bitcoin_reg_jwt:encode(jsx:encode(JData)),

      ?DEBUG("Token: ~p~n", [Token]),
      Resp = [{status, ok}, {data, [{token, Token}]}],
      {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
      {true, Req2, State};
    false ->
      Resp = [{status, error}, {message, <<"Invalid username, or password">>}],
      {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
      {false, Req2, State}
  end;

route(<<"PUT">>, _Path, Req, State) ->
  case State of
    undefined ->
      {ok, Req1} = cowboy_helper:json_reply([{error, no_such_user}], Req),
      {true, Req1, State};
    {_UserID, Rec} ->
      {ok, Body, Req1} = cowboy_req:body(Req),
      ?DEBUG("State: ~p, Data: ~p~n", [Rec, Body]),

      %% maybe bad JSON
      try jsx:decode(Body, [return_maps]) of
        Data ->
          %% check that supplied data should not contain password
          case maps:get(<<"password">>, Data, <<"">>) of
            <<"">> ->
              %% check if password needs updating
              case maps:get(<<"old_password">>, Data, <<"">>) of 
                <<"">> ->
                  %% no password
                  Data2 = [{binary_to_existing_atom(K, utf8), V} || {K,V} <- maps:to_list(Data)],
                  ListRec = ?record_to_tuplelist(btc_user, Rec),
                  ?DEBUG("ListRec: ~p~n", [ListRec]),
                  ?DEBUG("Data2: ~p~n", [Data2]),

                  %% merge, convert, and save the data
                  %%
                  Updated = list_merge(ListRec, Data2),
                  ?DEBUG("Updated List: ~p~n", [Updated]),

                  %% convert to record
                  Rec2 = ?tuplelist_to_record(btc_user, Updated),
                  ?DEBUG("Updated Rec: ~p~n", [Rec2]),

                  %% save the data
                  bitcoin_reg_db:save(Rec2),

                  %% reply ok
                  Resp = [{ok, updated}],
                  {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
                  {true, Req2, State};

                OldPass ->
                  %% ok, password updated is in progress
                  case util:hash(OldPass) =:= Rec#btc_user.password of 
                    true ->
                      %% ok, old password is good.
                      case maps:get(<<"new_password">>, Data, <<>>) of 
                        <<>> ->
                          %% return error. No new password.
                          Resp = [{error, <<"New password is mandatory">>}],
                          {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
                          {false, Req2, State};
                        NewPass ->
                          %% ok, hash and updated the rec
                          Updated = Rec#btc_user{password = util:hash(NewPass)},
                          bitcoin_reg_db:save(Updated),

                          Resp = [{ok, updated}],
                          {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
                          {true, Req2, State}
                      end;
                    false ->
                      Resp = [{error, <<"invalid username, or passwor">>}],
                      {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
                      {false, Req2, State}
                  end
              end;

            _ ->
              %% okay, password is supplied. 
              Resp = [{error, <<"To update password, supply old_password and new_password params">>}],
              {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
              {false, Req2, State}        
          end
      catch
        _:_ ->
          Resp = [{error, <<"Bad JSON data">>}],
          {ok, Req2} = cowboy_helper:json_reply(Resp, Req1),
          {false, Req2, State}        
      end
  end;

route(<<"GET">>, _Path, Req, State) ->
  {jsx:encode([{error, enoimpl}]), Req, State};

route(_Method, _Path, Req, State) ->
  {ok, Req1} = cowboy_helper:json_reply([{error, enoimpl}], Req),
  {false, Req1, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
list_merge(ListRec, [{K,V}|T]) ->
  L = lists:keyreplace(K, 1, ListRec, {K,V}),
  list_merge(L, T);

list_merge(ListRec, []) -> ListRec.

