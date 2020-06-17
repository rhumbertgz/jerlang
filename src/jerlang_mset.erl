-module(jerlang_mset).

-export([common_sets/2]).
-export([test_suite/0]).

-include("jerlang.hrl").

% -type(test_set_fun(B) ::
%       fun (([id()], {receive_fun_param(B), jdict(_)}) ->      
% 		  {'ok', [id()], B} | 'error')).

% -spec(common_sets/2 ::
%       ([[id()]], {test_set_fun(A), any(), id()}) ->
% 	     {error, no_match} | {ok, [id()], A}).
% -spec(test_suite/0 :: () -> 'ok' | no_return()).

-type test_set_fun(B) ::
      fun (([id()], {receive_fun_param(B), jdict(_)}) ->      
		  {'ok', [id()], B} | 'error').

-spec common_sets([[id()]], {test_set_fun(A), any(), id()}) ->
	     {error, no_match} | {ok, [id()], A}.
-spec test_suite() -> 'ok' | no_return().

common_sets(Patterns, Test) ->
    case common_sets(Patterns, [], [], Test) of
	{ok, _, _} = Result -> Result;
	{ok, _} -> {error, no_match}
    end.

%% FIXME: remove redundant code
common_sets([], Used, Result, {_, _, none}) ->
    Set = lists:reverse(Used),
    case lists:member(Set, Result) of
	true -> {no_test, Result};
	false -> {ok, [Set | Result]}
    end;
common_sets([], Used, Result, {_, _, Id}) ->
    Set  = lists:reverse(Used),
    case lists:member(Set, Result) 
	orelse (not lists:member(Id, Set)) of
	true ->  {no_test, Result};
	false -> {ok, [Set | Result]}
    end;
common_sets([Pattern | Rest], Used, Result, Test) ->
    common_sets_single(Pattern, Rest, Used, Result, Test).

common_sets_single([], _Rest, _Used, Result, _) ->
    {ok, Result};
common_sets_single([SingleElem | RestSet], Rest,
		   Used, Result, {Func, Arg, _}=Test) ->
    case lists:member(SingleElem, Used) of
	false ->
	    case common_sets(Rest, [SingleElem | Used],
					Result, Test) of
		{ok, []} ->
		    common_sets_single(RestSet, Rest, Used,
				       Result, Test);
		{ok, [Last | _] = NResult} ->
		    case Func(Last, Arg) of
			{ok, Last, FuncResult} ->
			    {ok, Last, FuncResult};
			error ->
			    common_sets_single(RestSet, Rest, Used,
					       NResult, Test);
			_ ->
			    %%  Keep Dialyzer happy.
			    %%  Otherwise complains about
			    %%  'no local return'.
			    exit({error, internal_error})
		    end;
		{no_test, Result} ->
		    common_sets_single(RestSet, Rest, Used,
				       Result, Test);
		FinalResult -> FinalResult
	    end;
	true ->
	    common_sets_single(RestSet, Rest, Used,
			       Result, Test)
    end.

test_suite() ->
    {ok, [1,2], [1,2]} = common_sets([[1,3],[2]],
				    {fun(Result, _Arg) ->
					    {ok, Result, Result}
				     end, [], 1}),
    {ok, [3,2], valid} = common_sets([[1,3],[2]],
				    {fun(Result, _Arg) ->
					     case Result of
						 [1,2] -> error;
						 _ -> {ok, Result, valid}
					     end
				     end, [], 2}),
    {ok, [3,2,1], ok} = common_sets([[1,2,3],[2,1],[1]],
				     {fun(Result, _Arg) ->
					     {ok, Result, ok}
				      end, [], 1}),
    {error, no_match} = common_sets([[1,2], [2,1], [1,2]],
			  {fun(Result, _Arg) ->
				  {ok, Result, ok}
			   end, [], 1}),
    ok.
