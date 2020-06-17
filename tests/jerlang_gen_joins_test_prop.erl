-module(jerlang_gen_joins_test_prop).
-behaviour(jerlang_gen_joins).

-export([init/1, init_joins/1, handle_join/2, terminate/0]).
-export([start/0, stop/0, auth/1, enter/1, revoke/1]).

%% FIXME: this should be added by parse_transform
-include("jerlang.hrl").

start() ->
    jerlang_gen_joins:start({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    ok.

terminate() ->
    ok.

init(_Args) ->
    {ok, {status, 2}}.

init_joins(_) ->
    {ok,
     [
      [[{1, #pattern_joins{test=
			   fun(Value) ->
				   case Value of
				       {auth, _} -> true;
				       _ -> false
				   end
			   end, msgs=[], prop=yes}},
	{2, #pattern_joins{test=
			   fun(Value) ->
				   case Value of
				       {enter, _} -> true;
				       _ -> false
				   end
			   end, msgs=[]}}],
       {fun([{auth, Name}, {enter, Name}], {status, Num})
	  when (Num > 0) ->
	       true
       end, []}],
      [[{3, #pattern_joins{test=
			   fun(Value) ->
				   case Value of
				       {auth, _} -> true;
				       _ -> false
				   end
			   end, msgs=[]}},
	{4, #pattern_joins{test=
			   fun(Value) ->
				   case Value of
				       {revoke, _} -> true;
				       _ -> false
				   end
			   end, msgs=[]}}],
       {fun([{auth, Name}, {revoke, Name}], _) ->
	       true
	end, []}]
      ]}.
      
auth(Name) ->
    jerlang_gen_joins:cast(?MODULE, {auth, Name}).

enter(Name) ->
    jerlang_gen_joins:call(?MODULE, {enter, Name}).

revoke(Name) ->
    jerlang_gen_joins:call(?MODULE, {revoke, Name}).

handle_join([{auth, Name}, {enter, Name}], {status, Num}) when (Num > 0) ->
    io:format("*Handle Join Auth Enter~n", []),
    {[noreply, {reply, valid_pass}], {status, Num}};
handle_join([{auth, Name}, {revoke, Name}], State) ->
    io:format("*Handle Join Revoke~n", []),
    {[noreply, {reply, {auth, Name}}], State}.
