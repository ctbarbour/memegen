-module(memebot_slack).

-include("memebot_slack.hrl").

-export([decode_slash_command/1]).

decode_slash_command(Body) ->
    build(proplist_from_form_urlencoded(Body)).

build(Command) ->
    build(Command, #slash_command{}).

build([], Command) ->
    Command;
build([{<<"token">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{token=Value});
build([{<<"team_id">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{team_id=Value});
build([{<<"team_domain">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{team_domain=Value});
build([{<<"channel_id">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{channel_id=Value});
build([{<<"channel_name">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{channel_name=Value});
build([{<<"user_id">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{user_id=Value});
build([{<<"user_name">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{user_name=Value});
build([{<<"command">>, Value} | Rest], Command) ->
    build(Rest, Command#slash_command{command=Value});
build([{<<"text">>, Value} | Rest], Command) ->
    DecodedText = decode_text(Value),
    build(Rest, Command#slash_command{text=DecodedText});
build([_Other | Rest], Command) ->
    build(Rest, Command).

proplist_from_form_urlencoded(Body) ->
   lists:map(fun(Pair) ->
		     [Key, Value] = binary:split(Pair, <<"=">>),
		     {Key, Value}
	     end, binary:split(Body, <<"&">>, [global])).

decode_text(Text) ->
    Replaced = binary:replace(Text, <<"+">>, <<"%20">>, [global]),
    http_uri:decode(binary_to_list(Replaced)).
