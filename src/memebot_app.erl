-module(memebot_app).

-behaviour(application).

-export([start/2, stop/1, on_request/1, on_response/4]).

start(_StartType, _StartArgs) ->
    ClientCredentials = client_credentials(),
    Dispatch = cowboy_router:compile([{'_',[{"/health", memebot_health_handler, []},
					    {"/slack", memebot_slack_handler, []},
					    {"/auth", memebot_auth_handler, ClientCredentials}
					   ]}]),
    {ok, _} = cowboy:start_http(http, 100,
				[{port, 8080}],
				[{env, [{dispatch, Dispatch}]},
				 {onrequest, fun ?MODULE:on_request/1},
				 {onresponse, fun ?MODULE:on_response/4}]),
    memebot_sup:start_link().

stop(_State) ->
    ok.

on_request(Req) ->
    ok = error_logger:info_msg("~p~n", [Req]),
    Req.

on_response(Code, Headers, Body, Req) ->
    ok = error_logger:info_msg("[~p] ~p~n~p~n", [Code, Headers, Body]),
    Req.

client_credentials() ->
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
    [{client_id, ClientId}, {client_secret, ClientSecret}].
