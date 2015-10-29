-module(memebot_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("eunit/include/eunit.hrl").

memebot_test_() ->
    {timeout, 60, ?_assert(run_props())}.

run_props() ->
    case proper:module(?MODULE, [long_result, {to_file, user}]) of
	[] ->
	    true;
	Result ->
	    ?debugFmt("~p~n", [Result]),
	    false
    end.

g_opts() ->
    ?LET({Top, Bottom}, {non_empty(binary()), non_empty(binary())},
	 [{top, Top}, {bottom, Bottom}]).

prop_generate() ->
    ?FORALL(Opts, g_opts(),
	    begin
		case memebot:generate(Opts) of
		    {ok, _Meme} ->
			true;
		    {error, _Error} ->
			false
		end
	    end).
