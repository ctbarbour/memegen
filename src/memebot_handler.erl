-module(memebot_handler).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([init/3,
	 handle/2,
	 terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = handle_method(cowboy_req:method(Req)),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_method({<<"GET">>, Req}) ->
    {Top, Req2} = cowboy_req:qs_val(<<"top">>, Req, <<"Meme?">>),
    {Bottom, Req3} = cowboy_req:qs_val(<<"bottom">>, Req2, <<"">>),
    {Source, Req3} = cowboy_req:qs_val(<<"source">>, Req2, <<"logo:">>),
    Opts = [{top, Top}, {bottom, Bottom}, {source, Source}],
    generate_meme(Opts, Req3);
handle_method({_, Req}) ->
    cowboy_req:reply(405, Req).

generate_meme(Opts, Req) ->
    {ok, Meme} = memebot:generate(Opts),
    Key = lists:flatten(io_lib:format("~s.jpg", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= erlang:md5(Meme) ]])),
    erlcloud_s3:put_object(bucket(), Key, Meme, [{acl, public_read}]),
    Config = erlcloud_aws:default_config(),
    GetUrl = erlcloud_s3:get_object_url(bucket(), Key, Config#aws_config{s3_scheme = "http://"}),
    Body = jsx:encode(#{<<"username">> => <<"memebot">>,
			<<"text">> => <<"">>,
			<<"icon_emoji">> => <<":ghost:">>,
		        <<"channel">> => <<"#dave-meme-gen">>,
		        <<"attachments">> => [
					      #{<<"image_url">> => list_to_binary(GetUrl)}
					     ]}),
    {ok, Result} = post_to_slack(Body),
    ok = error_logger:info_msg("~p~n", [Result]),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Body, Req).

bucket() ->
    {ok, Bucket} = application:get_env(memebot, s3_bucket),
    Bucket.

post_to_slack(Body) ->
    {ok, SlackURL} = application:get_env(memebot, slack_url),
    httpc:request(post, {SlackURL, [], "application/json", Body}, [], []).
