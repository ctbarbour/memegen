-module(memebot_handler).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([init/3,
	 handle/2,
	 terminate/3]).

-record(command, {
	  token,
	  team_id,
	  team_domain,
	  channel_id,
	  channel_name,
	  user_id,
	  user_name,
	  command,
	  text
	 }).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = handle_method(cowboy_req:method(Req)),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_method({<<"POST">>, Req}) ->
    {ok, Body, Req2} = cowboy_req:body(Req),
    Command = build_command(jsx:decode(Body), #command{}),
    generate_meme(Command, Req2);
handle_method({_, Req}) ->
    cowboy_req:reply(405, Req).

generate_meme(Command, Req) ->
    Opts = [{top, Command#command.text}, {source, filename:join(code:priv_dir(memebot), "ron-burgundy.jpg")}],
    {ok, Meme} = memebot:generate(Opts),
    Key = lists:flatten(io_lib:format("~s.jpg", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= erlang:md5(Meme) ]])),
    erlcloud_s3:put_object(bucket(), Key, Meme, [{acl, public_read}]),
    Config = erlcloud_aws:default_config(),
    GetUrl = erlcloud_s3:get_object_url(bucket(), Key, Config#aws_config{s3_scheme = "http://"}),
    Body = jsx:encode(#{<<"username">> => <<"memebot">>,
			<<"text">> => <<"">>,
			<<"icon_emoji">> => <<":ghost:">>,
		        <<"channel">> => Command#command.channel_name,
		        <<"attachments">> => [
					      #{<<"image_url">> => list_to_binary(GetUrl)}
					     ]}),
    {ok, Result} = post_to_slack(Body),
    ok = error_logger:info_msg("~p~n", [Result]),
    cowboy_req:reply(200, Req).

bucket() ->
    {ok, Bucket} = application:get_env(memebot, s3_bucket),
    Bucket.

build_command([], Command) ->
    Command;
build_command([{<<"user_name">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"channel_name">>, ChannelName} | Rest], Command) ->
    build_command(Rest, Command#command{channel_name=ChannelName});
build_command([{<<"text">>, Text} | Rest], Command) ->
    build_command(Rest, Command#command{text=Text});
build_command([_Other | Rest], Command) ->
    build_command(Rest, Command).

post_to_slack(Body) ->
    {ok, SlackURL} = application:get_env(memebot, slack_url),
    httpc:request(post, {SlackURL, [], "application/json", Body}, [], []).
