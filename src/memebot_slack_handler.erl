-module(memebot_slack_handler).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([init/3,
	 handle/2,
	 terminate/3]).

-record(command, {
	  token :: string(),
	  team_id :: string(),
	  team_domain :: string(),
	  channel_id  :: string(),
	  channel_name :: string(),
	  user_id :: string(),
	  user_name :: string(),
	  command :: string(),
	  text :: string()
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
    Command = build_command(parse_body(Body)),
    generate_meme(Command, Req2);
handle_method({_, Req}) ->
    cowboy_req:reply(405, Req).

generate_meme(Command, Req) ->
    Opts = [{top, Command#command.text}, {source, meme_source()}],
    {ok, Meme} = memebot:generate(Opts),
    GetUrl = put_object(Meme),
    Body = jsx:encode(#{<<"username">> => <<"memebot">>,
			<<"text">> => <<"">>,
			<<"icon_emoji">> => <<":ghost:">>,
		        <<"channel">> => Command#command.channel_id,
		        <<"attachments">> => [
					      #{<<"image_url">> => list_to_binary(GetUrl)}
					     ]}),
    {ok, Result} = post_to_slack(Body),
    cowboy_req:reply(200, Req).

meme_source() ->
    list_to_binary(filename:join(code:priv_dir(memebot), "ron-burgundy.jpg")).

bucket() ->
    {ok, Bucket} = application:get_env(memebot, s3_bucket),
    Bucket.

build_command(Command) ->
    build_command(Command, #command{}).

build_command([], Command) ->
    Command;
build_command([{<<"token">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"team_id">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"team_domain">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"channel_id">>, ChannelId} | Rest], Command) ->
    build_command(Rest, Command#command{channel_id=ChannelId});
build_command([{<<"channel_name">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"user_id">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"user_name">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"command">>, UserName} | Rest], Command) ->
    build_command(Rest, Command#command{user_name=UserName});
build_command([{<<"text">>, Text} | Rest], Command) ->
    StringText = list_to_binary(hd([http_uri:decode(StringText) ||
		     StringText <- [lists:flatten(io_lib:format("~s", [binary:replace(Text, <<"+">>, <<"%20">>, [global])]))]])),
    build_command(Rest, Command#command{text=StringText});
build_command([_Other | Rest], Command) ->
    build_command(Rest, Command).

post_to_slack(Body) ->
    SlackURL = os:getenv("SLACK_URL"),
    httpc:request(post, {SlackURL, [], "application/json", Body}, [], []).

parse_body(Body) ->
    lists:map(fun(Pair) ->
		      [Key, Value] = binary:split(Pair, <<"=">>),
		      {Key, Value}
	      end, binary:split(Body, <<"&">>, [global])).

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
