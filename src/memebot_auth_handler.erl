-module(memebot_auth_handler).

-export([init/3, handle/2, terminate/3]).

-record(state, {
	  slack :: memebot_slack:slack()
	 }).

-define(AUTH_REDIRECT_URI, "https://memebot.io/auth/").

init(_Type, Req, Opts) ->
    case lists:keyfind(slack, 1, Opts) of
	{slack, Slack} ->
	    {ok, Req, #state{slack=Slack}};
	false ->
	    {stop, missing_slack}
    end.

handle(Req, State) ->
    handle_method(cowboy_req:method(Req), State).

handle_method({<<"GET">>, Req}, State) ->
    case cowboy_req:qs_val(<<"code">>, Req, undefined) of
	{undefined, Req2} ->
	    request_code(Req2, State);
	{Code, Req2} ->
	    retrieve_token(Code, Req2, State)
    end;
handle_method({_Method, Req}, State) ->
    Req2 = cowboy_req:reply(405, Req),
    {ok, Req2, State}.

request_code(Req, State) ->
    #state{slack=Slack} = State,
    {AuthState, Req2} = cowboy_req:qs_val(<<"text">>, Req, <<"">>),
    AuthUrl = memebot_slack:oauth_authorize(client,
					    ?AUTH_REDIRECT_URI,
					    AuthState, Slack),
    {ok, Req2} = cowboy_req:reply(302, [{<<"location">>, AuthUrl}], Req),
    {ok, Req2, State}.

retrieve_token(Code, Req, State) ->
    #state{slack=Slack} = State,
    Response = memebot_slack:oauth_access(Code, ?AUTH_REDIRECT_URI, Slack),
    handle_slack_access_token(Response, Req, State).

handle_slack_access_token({ok, #{<<"access_token">> := AccessToken}}, Req, State) ->
    #state{slack=Slack} = State,
    case retrieve_user_id(AccessToken, Slack) of
	{ok, UserId} ->
	    ok = memebot_token_store:put(UserId, AccessToken),
	    {ok, Req2} = cowboy_req:reply(200, Req),
	    {ok, Req2, State};
	{error, Reason} ->
	    ok = error_logger:error_msg("Failed to retrieve user id from token: ~p~n",
					[Reason]),
	    {ok, Req2} = cowboy_req:reply(400, Req),
	    {ok, Req2, State}
	end;
handle_slack_access_token({error, Reason}, Req, State) ->
    ok = error_logger:error_msg("Failed to retrieve access token: ~s~n", [Reason]),
    Json = jsx:encode(#{
			 <<"ok">> => false,
			 <<"error">> => Reason
		       }),
    {ok, Req2} = cowboy_req:reply(400, [], Json, Req),
    {ok, Req2, State}.

retrieve_user_id(Token, Slack) ->
    case memebot_slack:auth_test(Token, Slack) of
	{ok, #{<<"user_id">> := UserId}} ->
	    {ok, UserId};
	{error, Reason} ->
	    {error, Reason}
    end.

terminate(_Reason, _Req, _State) ->
    ok.
