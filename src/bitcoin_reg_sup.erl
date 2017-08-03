-module(bitcoin_reg_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(WORKER(M,F,A), {M, {M, F, A}, permanent, 5000, worker, [M]}).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  JWT = ?WORKER(bitcoin_reg_jwt, start_link, []),
  DB = ?WORKER(bitcoin_reg_db, start_link, []),

  Procs = [JWT, DB],
  {ok, {{one_for_one, 1, 5}, Procs}}.
