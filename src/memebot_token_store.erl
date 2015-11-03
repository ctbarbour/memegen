-module(memebot_token_store).

-behavior(gen_server).

-include_lib("erlcloud/include/erlcloud_aws.hrl").

-export([start_link/2, put/2, get/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3,
	 terminate/2]).

-record(state, {
	  aws_region :: string(),
	  table :: binary(),
	  db_config :: aws_config()
	 }).

put(UserId, Token) ->
    gen_server:call(?MODULE, {put, UserId, Token}).

get(UserId) ->
    gen_server:call(?MODULE, {get, UserId}).

start_link(AwsRegion, Table) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE,
			  [AwsRegion, Table], []).

init([AwsRegion, Table]) ->
    DbConfig = erlcloud_aws:default_config(),
    DdbHost = lists:flatten(io_lib:format("dynamodb.~s.amazonaws.com", [AwsRegion])),
    DbConfig2 = DbConfig#aws_config{ddb_host=DdbHost},
    {ok, #state{aws_region=AwsRegion,
		table=Table,
		db_config=DbConfig2}}.

handle_call({put, UserId, Token}, _From, State) ->
    #state{db_config=DbConfig, table=Table} = State,
    Item = [{<<"userid">>, UserId}, {<<"access_token">>, Token}],
    case erlcloud_ddb2:put_item(Table, Item, [], DbConfig) of
	{ok, []} ->
	    {reply, ok, State};
	{error, Error} ->
	    ok = error_logger:error_msg("~p~n", [Error]),
	    {reply, {error, Error}, State}
    end;
handle_call({get, UserId}, _From, State) ->
    #state{db_config=DbConfig, table=Table} = State,
    case erlcloud_ddb2:get_item(Table, [{<<"userid">>, UserId}], [], DbConfig) of
	{ok, []} ->
	    {reply, {error, not_found}, State};
	{ok, Item} ->
	    {<<"access_token">>, Token} = lists:keyfind(<<"access_token">>, 1, Item),
	    {reply, {ok, Token}, State};
	{error, Error} ->
	    ok = error_logger:error_msg("~p~n", [Error]),
	    {reply, {error, Error}, State}
    end.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.
