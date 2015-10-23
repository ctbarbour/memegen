-module(memebot_handler).

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
    Key = lists:flatten(io_lib:format("~s", [[io_lib:format("~2.16.0B",[X]) || <<X:8>> <= erlang:md5(Meme) ]])),
    erlcloud_s3:put_object(bucket(), Key, Meme, [{acl, public_read}]),
    GetUrl = erlcloud_s3:make_get_url(86400, bucket(), Key),
    Body = jsx:encode(#{<<"url">> => list_to_binary(GetUrl)}),
    cowboy_req:reply(200, [{<<"content-type">>, <<"application/json">>}], Body, Req).

bucket() ->
    {ok, Bucket} = application:get_env(memebot, s3_bucket),
    Bucket.
