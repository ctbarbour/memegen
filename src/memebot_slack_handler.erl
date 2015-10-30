-module(memebot_slack_handler).

-include("memebot_slack.hrl").
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

handle_method({<<"POST">>, Req}) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Command = memebot_slack:decode_slash_command(Body),
    generate_meme(Command, Req2);
handle_method({_, Req}) ->
    cowboy_req:reply(405, Req).

generate_meme(Command, Req) ->
    Opts = [{top, Command#slash_command.text}, {source, meme_source()}],
    {ok, Meme} = memebot:generate(Opts),
    GetUrl = put_object(Meme),
    Body = jsx:encode(#{<<"username">> => <<"memebot">>,
			<<"text">> => <<"">>,
			<<"icon_emoji">> => <<":ghost:">>,
		        <<"channel">> => Command#slash_command.channel_id,
		        <<"attachments">> => [
					      #{<<"image_url">> => list_to_binary(GetUrl)}
					     ]}),
    {ok, Result} = post_to_slack(Body),
    ok = error_logger:info_msg("~p~n", [Result]),
    cowboy_req:reply(200, Req).

meme_source() ->
    list_to_binary(filename:join(code:priv_dir(memebot), "ron-burgundy.jpg")).

bucket() ->
    {ok, Bucket} = application:get_env(memebot, s3_bucket),
    Bucket.

post_to_slack(Body) ->
    SlackURL = os:getenv("SLACK_URL"),
    httpc:request(post, {SlackURL, [], "application/json", Body}, [], []).

put_object(Meme) ->
    MD5 = lists:flatten([io_lib:format("~2.16.0B", [X]) || <<X:8>> <= erlang:md5(Meme)]),
    Name = io_lib:format("~s.jpg", [MD5]),
    Key = filename:join(application:get_env(memebot, s3_prefix, "memes"), Name),
    Meta = [{"content-disposition", Name}, {"content-type", "image/jpg"},
	    {"content-md5", MD5}],
    Config = (erlcloud_aws:default_config())#aws_config{s3_host=s3_host()},
    _ = erlcloud_s3:put_object(bucket(), Key, Meme,
			       [{meta, Meta}], Config),
    ["https://memebot.io/memes/", Name].

s3_host() ->
    case os:getenv("AWS_REGION") of
	false ->
	    "s3.amazonaws.com";
	Region ->
	    lists:flatten(io_lib:format("s3-~s.amazonaws.com", [Region]))
    end.
