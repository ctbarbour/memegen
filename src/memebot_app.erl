-module(memebot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    {ok, ClientId} = case os:getenv("SLACK_CLIENT_ID") of
			 false ->
			     ok = error_logger:error_msg("Missing SLACK_CLIENT_ID environment variable"),
			     {error, missing_client_id};
			 Value ->
			     {ok, Value}
		     end,

    {ok, ClientSecret} = case os:getenv("SLACK_CLIENT_SECRET") of
			     false ->
				 ok = error_logger:error_msg("Missing SLACK_CLIENT_SECRET environment varibale"),
				 {error, missing_client_secret};
			     V ->
				 {ok, V}
			 end,

    Dispatch = cowboy_router:compile([{'_',[{"/health", memebot_health_handler, []},
					    {"/slack", memebot_slack_handler, []},
					    {"/auth", memebot_auth_handler, [{client_id, ClientId}, {client_secret, ClientSecret}]}
					   ]}]),
    {ok, _} = cowboy:start_http(http, 100,
				[{port, 8080}],
				[{env, [{dispatch, Dispatch}]}]),
    memebot_sup:start_link().

stop(_State) ->
    ok.
