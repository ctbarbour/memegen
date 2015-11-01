-module(memebot_sup).

-behaviour(supervisor).

-export([start_link/1]).
-export([init/1]).

start_link(AwsRegion) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE,
			  [AwsRegion]).

init([AwsRegion]) ->
    {ok, Table} = application:get_env(memebot, token_table),
    TokenStore = {memebot_token_store,
		  {memebot_token_store, start_link,
		   [AwsRegion, list_to_binary(Table)]},
		  permanent,
		  1000,
		  worker,
		  [memebot_token_store]},
    {ok, { {one_for_all, 0, 1}, [TokenStore]} }.
