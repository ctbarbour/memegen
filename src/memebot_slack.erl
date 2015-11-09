-module(memebot_slack).

-include("memebot_slack.hrl").

-export([new/2,
	 new/4,
	 new/5,
	 post_message/4,
	 oauth_access/3,
	 oauth_authorize/4,
	 auth_test/2,
	 decode_slash_command/1]).

-record(memebot_slack, {
	  api_uri :: string(),
	  oauth_uri :: string(),
	  client_id :: string(),
	  client_secret :: string(),
	  http_mod = httpc :: module()
	 }).

-opaque slack() :: #memebot_slack{}.

-export_type([slack/0]).

new(ClientId, ClientSecret) ->
    new(ClientId, ClientSecret, "https://slack.com/api/",
	"https://slack.com/oauth/").

new(ClientId, ClientSecret, SlackApi, OAuthUri) ->
    new(ClientId, ClientSecret, SlackApi, OAuthUri, httpc).

new(ClientId, ClientSecret, SlackApi, OAuthUri, HttpMod) ->
    #memebot_slack{api_uri=SlackApi, oauth_uri=OAuthUri,
		   client_id=ClientId, client_secret=ClientSecret,
		   http_mod=HttpMod}.

post_message(Text, ChannelId, Token, Slack) ->
    Params = [{as_user, true}, {token, Token}, {text, Text},
	      {channel, ChannelId}],
    rpc("chat.postMessage", Params, Slack).

oauth_authorize(Scope, RedirectUrl, State, Slack) ->
    #memebot_slack{oauth_uri=OAuthUri, client_id=ClientId} = Slack,
    Params = [{scope, Scope}, {redirect_url, RedirectUrl},
	      {client_id, ClientId}, {state, State}],
    uri_with_params(OAuthUri, "authorize", Params).

oauth_access(Code, RedirectUrl, Slack) ->
    #memebot_slack{client_id=ClientId, client_secret=ClientSecret} = Slack,
    Params = [{client_id, ClientId}, {client_secret, ClientSecret},
	      {code, Code}, {redirect_url, RedirectUrl}],
    rpc("oauth.access", Params, Slack).

auth_test(Token, Slack) ->
    rpc("auth.test", [{token, Token}], Slack).

rpc(Command, Params, Slack) ->
    #memebot_slack{api_uri=ApiUri} = Slack,
    Uri = uri_with_params(ApiUri, Command, Params),
    parse_http_response(http_request(Uri, Slack)).

http_request(Uri, Slack) when is_list(Uri) ->
    #memebot_slack{http_mod=HttpMod} = Slack,
    StringUri = lists:flatten(io_lib:format("~s", [Uri])),
    HttpMod:request(get, {StringUri, [{"accept", "application/json"}]},
		  [{autoredirect, true}, {url_encode, true}],
		  [{body_format, binary}, {full_result, false}]).

uri_with_params(Uri, Command, Params) ->
    Join = fun([H|T]) -> [H | [["&", X] || X <- T]] end,
    UrlParams = Join(lists:map(fun({Key, Value}) ->
				       [format_key_value(Key),
					"=",
					format_key_value(Value)]
			       end, Params)),
    lists:flatten([Uri, Command, "?", UrlParams]).

format_key_value(Key) when is_atom(Key) ->
    format_key_value(atom_to_list(Key));
format_key_value(Key) when is_binary(Key) ->
    format_key_value(binary_to_list(Key));
format_key_value(Key) when is_list(Key) ->
    http_uri:encode(Key).

parse_http_response({ok, {Code, Body}})
  when Code >= 200 andalso Code < 300 ->
    handle_http_success(Body);
parse_http_response({ok, {Code, Body}})
  when Code >= 300 ->
    handle_http_error(Body);
parse_http_response({error, Reason}) ->
    {error, Reason}.

handle_http_success(Body)
  when is_binary(Body) ->
    {ok, jsx:decode(Body, [return_maps])}.

handle_http_error(Body)
  when is_binary(Body) ->
    case jsx:is_json(Body) of
	true ->
	    Json = jsx:decode(Body, [return_maps]),
	    parse_slack_api_response(Json);
	false  ->
	    {error, Body}
    end.

parse_slack_api_response(#{<<"ok">> := true} = Response) ->
    {ok, Response};
parse_slack_api_response(#{<<"ok">> := false, <<"error">> := Error}) ->
    {error, Error}.

decode_slash_command(Body) ->
    build(cow_qs:parse_qs(Body)).

build(Command) ->
    build(Command, #slash_command{}).

build([], Command) ->
    Command;
build([{<<"token">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{token=Value});
build([{<<"team_id">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{team_id=Value});
build([{<<"team_domain">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{team_domain=Value});
build([{<<"channel_id">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{channel_id=Value});
build([{<<"channel_name">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{channel_name=Value});
build([{<<"user_id">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{user_id=Value});
build([{<<"user_name">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{user_name=Value});
build([{<<"command">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{command=Value});
build([{<<"text">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{text=Value});
build([{<<"response_url">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{response_url=Value});
build([_Other | Rest], Command) ->
    build(Rest, Command).
