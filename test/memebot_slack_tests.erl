-module(memebot_slack_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

-define(client_id, <<"ClientId">>).
-define(client_secret, <<"ClientSecret">>).
-define(slack_api_uri, <<"https://example.com/api/">>).
-define(slack_oauth_uri, <<"https://example.com/oauth/">>).

memebot_slack_test_() ->
    {timeout, 60, ?_assert(run_props())}.

memebot_slack_post_message_test() ->
    _ = meck:new(mock_httpc, [non_strict]),
    ok = meck:expect(mock_httpc, request,
		     fun(get, _Request, _HttpOptions, _Options) ->
			     {ok, {200, jsx:encode(#{<<"ok">> => true})}}
		     end),
    Slack = memebot_slack:new(?client_id, ?client_secret,
			      ?slack_api_uri, ?slack_oauth_uri,
			      mock_httpc),
    ?assertMatch({ok, #{<<"ok">> := true}},
		 memebot_slack:post_message(<<"Text">>, <<"ChanneId">>, <<"Token">>, Slack)),
    ok = meck:unload(mock_httpc).

memebot_slack_post_message_error_test() ->
    _ = meck:new(mock_httpc, [non_strict]),
    ok = meck:expect(mock_httpc, request,
		     fun(get, _Request, _HttpOptions, _Options) ->
			     {ok, {400, jsx:encode(#{<<"ok">> => false,
						     <<"error">> => "Error Message"})}}
		     end),
    Slack = memebot_slack:new(?client_id, ?client_secret, ?slack_api_uri,
			      ?slack_oauth_uri, mock_httpc),
    ?assertMatch({error, "Error Message"},
		 memebot_slack:post_message(<<"Text">>, <<"ChannelId">>, <<"Token">>, Slack)),
    ok = meck:unload(mock_httpc).

run_props() ->
    case proper:module(?MODULE, [long_result, {to_file, user}]) of
	[] ->
	    true;
	Result  ->
	    ?debugFmt("~p~n", [Result]),
	    false
    end.

ascii_char() ->
    integer(0,255).

g_url_attribute(Key, Gen) ->
    ?LET({K, V},
	 {exactly(Key), Gen},
	 [K, $=, http_uri:encode(V)]).

join_url_params([H|T]) ->
    [H | [["&", X] || X <- T]].

g_slash_command() ->
    ?LET(Params,
	 [g_url_attribute(Key, Value) ||
	     {Key, Value} <- [{"token", vector(24, ascii_char())},
			      {"team_id", vector(9, ascii_char())},
			      {"team_domain", list(ascii_char())},
			      {"channel_id", list(ascii_char())},
			      {"channel_name", list(ascii_char())},
			      {"user_id", vector(9, ascii_char())},
			      {"user_name", list(ascii_char())},
			      {"command", list(ascii_char())},
			      {"text", list(ascii_char())}]],
	 iolist_to_binary(join_url_params(Params))).

prop_decode_slash_command() ->
    ?FORALL(SlashCommand, g_slash_command(),
	    begin
		_ = memebot_slack:decode_slash_command(SlashCommand),
		true
	    end).
