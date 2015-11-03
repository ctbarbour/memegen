-module(memebot_slack_handler).

-include("memebot_slack.hrl").
-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([init/3,
	 handle/2,
	 terminate/3]).

-record(state, { slack :: memebot_slack:slack() }).

init(_Type, Req, [Slack]) ->
    {ok, Req, #state{slack=Slack}}.

handle(Req, State) ->
    {ok, Req2} = handle_method(cowboy_req:method(Req), State),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_method({<<"POST">>, Req}, State) ->
    #state{slack=Slack} = State,
    {ok, Body, Req2} = cowboy_req:body(Req),
    Command = memebot_slack:decode_slash_command(Body),
    case memebot_token_store:get(Command#slash_command.user_id) of
	{ok, Token} ->
	    {ok, GetUrl} = generate_meme(Command),
	    case memebot_slack:post_message(GetUrl, Command#slash_command.channel_id, Token, Slack) of
		{ok, _Response} ->
		    cowboy_req:reply(204, Req2);
		{error, Reason} ->
		    ok = error_logger:error_msg("Failed to post message to slack: ~p~n", [Reason]),
		    Response = ["Need to authenticate before you can use <https://memebot.io/auth?text=", http_uri:encode(Command#slash_command.text), "|memebot.io>"],
		    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Response, Req2)
	    end;
	_ ->
	    Response = ["Need to authenticate before you can use <https://memebot.io/auth?text=", http_uri:encode(Command#slash_command.text), "|memebot.io>"],
	    cowboy_req:reply(200, [{<<"content-type">>, <<"text/plain">>}], Response, Req2)
    end;
handle_method({_, Req}, _State) ->
    cowboy_req:reply(405, Req).

generate_meme(Command) ->
    Opts = [{top, Command#slash_command.text}, {source, meme_source()}],
    {ok, Meme} = memebot:generate(Opts),
    GetUrl = put_object(Meme),
    {ok, GetUrl}.

meme_source() ->
    filename:join(code:priv_dir(memebot), "ron-burgundy.jpg").

bucket() ->
    {ok, Bucket} = application:get_env(memebot, s3_bucket),
    Bucket.

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
