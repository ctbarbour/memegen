-module(memebot_slack).

-include("memebot_slack.hrl").

-export([new/2,
	 new/3,
	 post_message/4,
	 oauth_access/3,
	 oauth_authorize/4,
	 decode_slash_command/1]).

-record(memebot_slack, {
	  api_uri :: string(),
	  client_id :: string(),
	  client_secret :: string()
	 }).

-opaque slack() :: #memebot_slack{}.

-export_type([slack/0]).

new(ClientId, ClientSecret) ->
    new(ClientId, ClientSecret, "https://slack.com/api/").

new(ClientId, ClientSecret, SlackApi) ->
    #memebot_slack{api_uri=SlackApi, client_id=ClientId,
		   client_secret=ClientSecret}.

post_message(Text, ChannelId, Token, Slack) ->
    Params = [{as_user, true}, {token, Token}, {text, Text},
	      {channel, ChannelId}],
    rpc("chat.postMessage", Params, Slack).

oauth_authorize(Scope, RedirectUrl, State, Slack) ->
    #memebot_slack{client_id=ClientId} = Slack,
    Params = [{scope, Scope}, {redirect_url, RedirectUrl},
	      {client_id, ClientId}, {state, State}],
    uri_with_params("https://slack.com/oauth/", "authorize", Params).

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
    ok = error_logger:info_msg("SLACK RPC: ~s~n", [Uri]),
    parse_http_response(httpc:request(get,
				      {Uri, [{"accept", "application/json"}]},
				      [{autoredirect, true},
				       {url_encode, true}],
				      [{body_format, binary},
				       {full_result, false}])).

uri_with_params(Uri, Command, Params) ->
    Join = fun([H|T]) -> lists:flatten([H | [["&", X] || X <- T]]) end,
    UrlParams = Join(lists:map(fun({Key, Value}) ->
				       [atom_to_list(Key), "=", Value]
			       end, Params)),
    lists:flatten([Uri, Command, "?", UrlParams]).

parse_http_response({ok, {Code, Body}})
  when Code >= 200 andalso Code < 300 ->
    handle_http_success(Body);
parse_http_response({ok, {Code, Body}})
  when Code >= 300 ->
    ok = error_logger:warning_msg("[~d] ~p~n", [Code, Body]),
    handle_http_error(Body);
parse_http_response({error, Reason}) ->
    ok = error_logger:error_msg("Failed to send rpc request. ~p~n", [Reason]),
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
    build(proplist_from_form_urlencoded(Body)).

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
    DecodedText = decode_text(Value),
    build(Rest, Command#slash_command{text=DecodedText});
build([_Other | Rest], Command) ->
    build(Rest, Command).

proplist_from_form_urlencoded(Body) ->
   lists:map(fun(Pair) ->
		     [Key, Value] = binary:split(Pair, <<"=">>),
		     {Key, Value}
	     end, binary:split(Body, <<"&">>, [global])).

decode_text(Text) ->
    Replaced = binary:replace(Text, <<"+">>, <<"%20">>, [global]),
    http_uri:decode(binary_to_list(Replaced)).
