-module(memebot_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([{'_',[{"/health", memebot_health_handler, []},
					    {"/", memebot_handler, []}]}]),
    {ok, _} = cowboy:start_http(http, 100,
				[{port, 8080}],
				[{env, [{dispatch, Dispatch}]}]),
    memebot_sup:start_link().

stop(_State) ->
    ok.
