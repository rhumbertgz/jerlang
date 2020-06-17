-module(jerlang_gen_joins_mset).

-export([beta/3]).

-include("jerlang.hrl").


% -type(fun_msg() :: fun(([id()]) -> msg())).
% -type(opt() :: {status(), [id()],
% 		{fun_beta_test(), state(), fun_msg()},
% 		dupl()}).
% -spec(beta/3 :: ([[id()]], [id()], opt()) ->
% 	    {'ok', [id()]}).

-type fun_msg() :: fun(([id()]) -> msg()).
-type opt() :: {status(), [id()],
		{fun_beta_test(), state(), fun_msg()},
		dupl()}.
-spec beta([[id()]], [id()], opt()) ->
	    {'ok', [id()]}.

%%  -----------------------------------------
%%  Main function that runs the beta tests
%%  for Rete algorithm. Trying to find a
%%  unique set that satisfies the conditions.
%%  Can be either final or partial reduction.
%%  To improve the efficiency of the algorithm
%%  for large amounts of data, we purge branches
%%  that cannot be satisfied (for example, by
%%  removing entries that are duplicates, if they
%%  fail).
beta(Previous, Pattern, Opt) ->
    beta0(Previous, Pattern, [], Opt).

beta0(_, [], _, _) ->
    {ok, []};
beta0([], _, Res, _) ->
    {ok, lists:reverse(Res)};
beta0([P | Rest], Matches0, _,
      {final, _, _, D}=Opt) ->

    Matches1 = Matches0 -- P,
    %% No need to run purge_current in final
    %% since we are only interested in the first correct
    %% and there are no previous entries.
%    io:format("FINAL REDUCTION [~p ~p]~n", [P, Matches1]),
    Result0 = beta1(P, Matches1, [], Opt),
    case Result0 of
	[] ->
	    NRest = purge(P, Rest, D),
	    beta0(NRest, Matches0, [], Opt);
	_ ->
%	    io:format("Finalize...~n", []),
	    {ok, Result0}
    end;
beta0([P | Rest], Matches0, Res,
      {partial, Current, _, D}=Opt) ->
    Matches1 = Matches0 -- P,
    Matches2 = purge_current(Current, Matches1, D),

    Result0 = beta1(P, Matches2, [], Opt),
    NRest = purge(P, Rest, D),
    beta0(NRest, Matches0, Result0 ++ Res, Opt).

beta1(_, [], Res, _) ->
    lists:reverse(Res);
beta1(P, [E | Rest], Res,
      {Status, Current, {Fun, State, FunIds}, Dupl}=Opt) ->
    Term = P ++ [E],
    case lists:member(Term, Current) or lists:member(E, P) of
	false ->
%	    io:format("BBB2 ~p~n", [Term]),
	    Msgs = FunIds(Term),
	    try Fun(Msgs, State) of
		    true ->
		        beta_action(Term, Status,
				    {E, P, Rest, Res, Opt})
	    catch
		error:function_clause ->
		    NRest = purge(E, Rest, Dupl),
		    beta1(P, NRest, Res, Opt)
	    end;
	_ ->
%	    io:format("BBB1~n", []),
	    beta1(P, Rest, Res, Opt)
    end.

beta_action(Found, final, _) ->
    Found;
beta_action(Found, partial,
	    {Elem, LeftPartial, Right,
	     Res, {_, _, _, D}=Opt}) ->
    NRight = purge(Elem, Right, D),
    beta1(LeftPartial, NRight, [Found | Res], Opt).

purge(E, List, Duplicates) ->
    {ok, NList} = jerlang_2dict:get(E, List, Duplicates),
%    {_S1, S2} = Duplicates,
%    io:format("DUPL: ~p~n", [dict:to_list(S2)]),
%    io:format("PURGE [~p]: ~p and ~p~n", [E, List, NList]),
    NList.

%%  Remove entries from the existing matches
%%  that would produce the same results
purge_current(_, [], _) ->
    [];
purge_current([], Matches, _) ->
    Matches;
purge_current([E | Rest], Matches, D) ->
    %% FIXME: we should have a possibility of 
    %% of storing the tail somewhere?
    %% this computation will be a bottleneck,
    %% for larger number of joins
    %% (not recommended?).
    Tail = tail(E),
    Res = purge(Tail, Matches, D),
    purge_current(Rest, Res, D).    

tail([]) ->
    error;
tail([E]) ->
    E;
tail([_E | Rest]) ->
    tail(Rest).
