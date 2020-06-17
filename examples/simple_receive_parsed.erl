-module(simple_receive_parsed).

-include("jerlang.hrl").

-export([start/0]).

start() ->
    jerlang:loop(
      [
       [{1, #pattern_joins{test=
			   fun(Value1) ->
				   case Value1 of
				       {atomic, _Value} -> true;
				       _ -> false
				   end
			   end, msgs=[]}},
	{2, #pattern_joins{test=
			   fun(Value2) ->
				   case Value2 of
				       {test, _Value} -> true;
				       _ -> false
				   end
			   end}}],
       [{3, #pattern_joins{test=
			   fun(Value3) ->
				   case Value3 of
				       {commit, _TransId} -> true;
				       _ -> false
				   end
			   end, msgs=[]}}]],
      [fun(Res1, Case1) ->
	       [{atomic, Value}, {test, Value}] = Res1,
	       Ret = true,
	       case Case1 of
		   test_entry ->
		       Ret;
		   run_all ->
		       {ok, Value}
	       end
       end,
       fun(Res2, Case2) ->
	       [{commit, TransId}] = Res2,
	       Ret = (TransId > 0),
	       case Case2 of
		   test_entry -> 
		       Ret;
		   run_all ->
		       {ok, {trans, TransId}}
	       end
       end],
      [{timeout, 10000,
	fun() ->
		{error, timeout}
	end}]).

%    receive
%	{atomic, Value} and {test, Value} ->
%	    {ok, Value};
%	{commit, TransId} when (TransId > 0)  ->
%	    {ok, {trans, TransId}}
%    after 10000 ->
%	    {error, timeout}
%    end.
