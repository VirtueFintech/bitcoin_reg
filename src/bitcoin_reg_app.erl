-module(bitcoin_reg_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

-export ([route/0]).
-export ([reload/0]).

start(_Type, _Args) ->
  Dispatch = cowboy_router:compile(route()),
  RanchOptions = [{port, 8080}],
  CowboyOptions = [
    {env, [{dispatch, Dispatch}]},
    {middlewares, [cowboy_router, middleware_cors, cowboy_handler]},
    {compress, true},
    {timeout, 12000}
  ],  
  {ok, _} = cowboy:start_http(bitcoin_reg, 100, RanchOptions, CowboyOptions),
  bitcoin_reg_sup:start_link().

stop(_State) ->
  ok.

route() ->
  [
    {'_', [
      {"/", handler_main, #{}},
      
      {"/api/v1/auth", handler_auth, #{}},
      {"/api/v1/auth/:username", handler_auth, #{}},

      {"/api/v1/register/[...]", handler_btcreg, #{}}
    ]}
  ].

reload() ->
  cowboy:set_env(bitcoin_reg, dispatch,
    cowboy_router:compile(route())).

