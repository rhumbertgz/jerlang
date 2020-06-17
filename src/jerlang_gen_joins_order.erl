-module(jerlang_gen_joins_order).

-export([optimize/3]).
-import(jerlang_misc, [busy_expr/1]).

optimize(P0, Id, {Status, Guard}) ->
    Parse = fun(Expr) ->
		    sets:from_list(
		      dict:fetch_keys(busy_expr(Expr)))
	    end,
    {P1, Initial} = init(P0, Id, Parse),
    VStatus = Parse(Status),
    VGuard = Parse(Guard),
    P2 = dependencies(P1, VStatus, VGuard),
%    io:format("This is the ranking: ~p~n", [dict:to_list(P2)]),
    Res = sort_rank(P2),
    case Res of
	Initial ->
	    {ok, valid};
	_ ->
	    {ok, generate_order(Res, [], P2, [])}
    end.

%% --- store the patterns in dict
init(P, Id0, Fun) ->
    {_, Dict} = 
	lists:foldl(
	  fun(E, {Id, S}) ->
		  {Id + 1, dict:store(Id, {Fun(E), E}, S)}
	  end, {Id0, dict:new()}, P),
    {Dict, [X || X <- lists:seq(Id0, Id0 + (length(P) -1))]}.

%% --- determine rank of each pattern
dependencies(Dict, VStatus, VGuard) ->
    dict:map(
      fun(Key, {Vars, E}) ->
	      Rank = dependency(Key, Vars, Dict),
	      %% Do we treat guards and status the same?
	      %% I guess so.
	      {Vars,
	       {Rank, has_common_set(Vars, VStatus),
		has_common_set(Vars, VGuard)},
	       E}
      end, Dict).

dependency(Id, Vars, D) ->
    dependency(Id, Vars, dict:to_list(D), []).

dependency(_, _, [], Result) ->
    lists:reverse(Result);
dependency(Id, Vars, [{Id, _} | Rest], Result) ->
    dependency(Id, Vars, Rest, Result);
dependency(Id0, Vars0, [{Id1, {Vars1, _}} | Rest], RankList) ->
    NRankList = 
	case has_common_set(Vars0, Vars1) of
	    true ->
		[Id1 | RankList];
	    _ ->
		RankList
	end,
    dependency(Id0, Vars0, Rest, NRankList).

has_common_set(Vars0, Vars1) ->
    sets:size(sets:intersection(Vars0, Vars1)) > 0.

sort_rank(Dict) ->
    L0 = dict:to_list(Dict),
    L1 = lists:keysort(1, L0),
    L2 = lists:sort(
	   fun({_, {_, Rank0, _}}, {_, {_, Rank1, _}}) ->
		   compare_rank(Rank0, Rank1)
	   end, L1),
    lists:map(
      fun({Id, _}) ->
	      Id
      end, L2).

compare_rank({L1, _, _}, {L2, _, _}) when (length(L1) > length(L2)) ->
    true;
compare_rank({L1, _, _}, {L2, _, _}) when (length(L1) < length(L2)) ->
    false;
compare_rank({_, VS1, VG1}, {_, VS2, VG2}) ->
    case compare_static(VS1, VS2) of
	{ok, L} -> L;
	error ->
	    case compare_static(VG1, VG2) of
		{ok, L} -> L;
		error -> true
	    end
    end.

compare_static(false, true) ->
    {ok, false};
compare_static(true, false) ->
    {ok, true};
compare_static(_, _) ->
    error.

generate_order([], [], _, Result) ->
    lists:reverse(Result);
generate_order(Main, [P | Rest], Dict, Result) ->
    {_, {Joined, _, _}, _} = dict:fetch(P, Dict),
    NJoined = available(Joined, Main),
    generate_order(Main -- Joined, Rest ++ NJoined,
		   Dict, [P | Result]);
generate_order([P | Rest], [], Dict, Result) ->
    {_, {Joined, _, _}, _} = dict:fetch(P, Dict),
    NJoined = available(Joined, Rest),
    generate_order(Rest -- Joined, NJoined, Dict, [P | Result]).

available(Required, All) ->
    Inter = sets:intersection(
	      sets:from_list(Required),
	      sets:from_list(All)),
    sets:to_list(Inter).
