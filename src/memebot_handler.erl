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
    {Text, Req2} = cowboy_req:qs_val(<<"t">>, Req, <<"Meme?">>),
    {Source, Req3} = cowboy_req:qs_val(<<"s">>, Req2, <<"logo:">>),
    Opts = [{text, Text}, {source, Source}],
    generate_meme(Opts, Req3);
handle_method({_, Req}) ->
    cowboy_req:reply(405, Req).

generate_meme(Opts, Req) ->
    {ok, Meme} = memebot:generate(Opts),
    cowboy_req:reply(200, [{<<"content-type">>, <<"image/jpeg">>}], Meme, Req).
