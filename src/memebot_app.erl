-module(memebot_app).

-behaviour(application).

-export([start/2, stop/1, on_request/1, on_response/4]).

start(_StartType, _StartArgs) ->
    {ok, AwsRegion} = get_env("AWS_REGION"),
    Slack = client_credentials(),
    Dispatch = cowboy_router:compile([{'_',[{"/health", memebot_health_handler, []},
					    {"/slack", memebot_slack_handler, [Slack]},
					    {"/auth", memebot_auth_handler, [Slack]}
					   ]}]),
    {ok, _} = cowboy:start_http(http, 100,
				[{port, 8080}],
				[{env, [{dispatch, Dispatch}]},
				 {onrequest, fun ?MODULE:on_request/1},
				 {onresponse, fun ?MODULE:on_response/4}]),
    memebot_sup:start_link(AwsRegion).

stop(_State) ->
    ok.

on_request(Req) ->
    ok = error_logger:info_msg("~p~n", [Req]),
    Req.

on_response(Code, Headers, Body, Req) ->
    ok = error_logger:info_msg("[~p] ~p~n~p~n", [Code, Headers, Body]),
    Req.

get_env(Env) ->
    case os:getenv(Env) of
	false ->
	    ok = error_logger:error_msg("Missing ~s environment variable", [Env]),
	    {error, {missing_environment_variable, Env}};
	Val ->
	    {ok, Val}
    end.

client_credentials() ->
    {ok, ClientId} = get_env("SLACK_CLIENT_ID"),
    {ok, ClientSecret} = get_env("SLACK_CLIENT_SECRET"),
    memebot_slack:new(ClientId, ClientSecret).
