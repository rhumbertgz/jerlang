-module(jerlang_gen_joins_test_example).
-behaviour(jerlang_gen_joins).

-compile({parse_transform, jerlang_gen_joins_parse}).

-export([init/1, handle_join/2, terminate/0]).
-export([start/0, add/1, multiply/1, divide/1, number/2]).

start() ->
    jerlang_gen_joins:start({local, ?MODULE}, ?MODULE, [], []).

terminate() ->
    jerlang_gen_joins:call(?MODULE, stop).

init(_Args) ->
    {ok, 0}.

number(Number, Id) ->
    jerlang_gen_joins:cast(?MODULE, {number, Id, Number}).

add(Id) ->
    jerlang_gen_joins:call(?MODULE, {add, Id}).

multiply(Id) ->
    jerlang_gen_joins:call(?MODULE, {multiply, Id}).

divide(Id) ->
    jerlang_gen_joins:call(?MODULE, {divide, Id}).

handle_join({number, Id, First} and {number, Id, Second}
	    and {number, Id, Third} and {add, Id}, Status) ->

    io:format("[gen_joins]: Calculate 'add' operation~n", []),
    {[noreply, noreply, {reply, First + Second + Third}],
      Status};
handle_join({number, Id, First} and {number, Id, Second}
	    and {multiply, Id}, Status) ->

    io:format("[gen_joins]: Calculate 'multiply' operation~n", []),
    {[noreply, noreply, {reply, First * Second}], Status};
handle_join({number, Id, First} and {number, Id, Second}
	    and {divide, Id}, Status) when (Second =/= 0) ->

    io:format("[gen_joins]: Calculate 'divide' operation~n", []),
    {[noreply, noreply, {reply, First / Second}],
      Status};
handle_join({number, Id, _} and {number, Id, Second}
	    and {divide, Id}, Status) when (Second =:= 0) ->
    io:format("[gen_joins]: Invalid divide operation~n", []),
    {[noreply, noreply, {reply, {error, division_by_zero}}],
      Status + 1}.
