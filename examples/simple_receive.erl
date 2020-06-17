-module(simple_receive).

-compile({parse_transform, jerlang_parse}).

-export([start/0]).

start() ->
    receive
	{atomic, Value} and {test, Value} ->
	    {ok, Value};
	{commit, TransId} when (TransId > 0)  ->
	    {ok, {trans, TransId}}
    after 10000 ->
	    {error, timeout}
    end.
