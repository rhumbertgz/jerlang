-module(jerlang_gen_joins_test_prop_parse).
-compile({parse_transform, jerlang_gen_joins_parse}).
-behaviour(jerlang_gen_joins).

-record(test, {name, price}).

-export([init/1, handle_join/2, terminate/0]).
-export([start/0, stop/0, auth/1, enter/1, revoke/1]).

start() ->
    jerlang_gen_joins:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    ok.

terminate() ->
    ok.

init(_Args) ->
    {ok, {status, 2}}.

auth(Name) ->
    jerlang_gen_joins:cast(?MODULE, {auth, Name}).

enter(Name) ->
    jerlang_gen_joins:call(?MODULE, {enter, Name}).

revoke(Name) ->
    jerlang_gen_joins:call(?MODULE, {revoke, Name}).

handle_join({auth, Name} and {enter, Name}, {status, Num}) when (Num > 0) ->
    io:format("*Handle Join Auth Enter~n", []),
    {[noreply, {reply, valid_pass}], {status, Num}};
handle_join({auth, Name} and {revoke, Name}, State) ->
    io:format("*Handle Join Revoke~n", []),
    {[noreply, {reply, {auth, Name}}], State};
%% FIXME: just for experimenting patterns
handle_join({value, A, F} and {value, _} and [#test{name=A, price=C} | _]
	   and {ok, size, D} and {merge, C, E, F},
	   {dict, D}) when (A < E) ->
    {[noreply, noreply, noreply, {reply, ok},
      noreply], {dict, D}}.
