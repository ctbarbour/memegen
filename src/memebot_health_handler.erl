-module(memebot_health_handler).

-export([init/3, handle/2, terminate/3]).

init(_Type, Req, _Opts) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    {ok, Req2} = handle_method(cowboy_req:method(Req)),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.

handle_method({<<"GET">>, Req}) ->
    cowboy_req:reply(200, Req).
