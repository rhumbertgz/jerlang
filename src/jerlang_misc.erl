-module(jerlang_misc).

-compile(nowarn_export_all). %% ADDED
-compile(export_all).

-include("jerlang.hrl").

% -spec(convert_now/1 :: ({byte(), byte(), byte()}) -> byte()).
% -spec(unique_atom/1 :: (string()) -> atom()).
% -spec(parse_list/2 ::
%       (erl_term(), integer()) ->
% 	     {'nil', integer()} |
% 	     {'cons', integer(), erl_term(), erl_term()}).
% -spec(unbounded_var/1 :: (atom() | string()) -> atom()).
% -spec(unbounded_var_null/1 :: (integer()) -> erl_term()).
% -spec(is_unbounded_var/1 :: (atom() | [byte()]) -> bool()).

-spec convert_now({byte(), byte(), byte()}) -> byte().
-spec unique_atom(string()) -> atom().
-spec parse_list(erl_term(), integer()) ->
	     {'nil', integer()} |
	     {'cons', integer(), erl_term(), erl_term()}.
-spec unbounded_var(atom() | string()) -> atom().
-spec unbounded_var_null(integer()) -> erl_term().
-spec is_unbounded_var(atom() | [byte()]) -> boolean().

merge_into_lists([], Result) ->
    Result;
merge_into_lists([{Key, Elem} | Rest], Result) ->
    Res = case lists:keytake(Key, 1, Result) of
	      {value, {Key, List}, RestResult} ->
		  [{Key, [Elem | List]} | RestResult];
	      false ->
		  [{Key, [Elem]} | Result]
    end,
    merge_into_lists(Rest, Res).

convert_now({MegaSecs,Secs,MicroSecs}) ->
    (MegaSecs*1000000 + Secs)*1000000 + MicroSecs.

unique_atom(Base) ->
    list_to_atom(Base ++ integer_to_list(convert_now(erlang:timestamp()))).

unique_vars(Base, Length) when (is_atom(Base)) ->
    unique_vars(atom_to_list(Base), Length);
unique_vars(Base, Length) ->
    [list_to_atom(Base ++ integer_to_list(X)) || X <- lists:seq(1,Length)].

parse_list([], Line) ->
    {nil, Line};
parse_list([Elem | []], Line) ->
    {cons, Line, Elem, parse_list([], Line)};
parse_list([Elem | Rest], Line) ->
    RFirst = parse_list(Elem, Line),
    RSecond = parse_list(Rest, Line),
    {cons, Line, RFirst, RSecond};
parse_list(Elem, _Line) ->
    Elem.

unbounded_var(VarNameAtom) when is_atom(VarNameAtom) ->
    unbounded_var(atom_to_list(VarNameAtom));
unbounded_var(VarNameList) when is_list(VarNameList) ->
    case is_unbounded_var(VarNameList) of
	true -> list_to_atom(VarNameList);
	false -> list_to_atom("_" ++ VarNameList)
    end.

unbounded_var_null(Line) ->
    {var, Line, '_'}.

is_unbounded_var(VarNameAtom) when is_atom(VarNameAtom) ->
    is_unbounded_var(atom_to_list(VarNameAtom));
is_unbounded_var([95 | _]) ->
    true;
is_unbounded_var(_) ->
    false.

generate_seed(Seed) ->
    {A1, A2, A3} = erlang:timestamp(),
    rand:seed(A1+Seed, A2*Seed, A3),
    ok.

%% Statistics
average(List) ->
    round(lists:foldl(fun(X, Sum) -> X + Sum end, 0, List) / length(List)).

%%  ---------------- Parsing functions ----------------

busy_expr(Expr) ->
    busy_expr(Expr, dict:new()).

busy_expr(Expr, Vars) when is_list(Expr) ->
    busy_expr0(Expr, Vars);
busy_expr({clauses, Clauses}, Vars) ->
    busy_expr(Clauses, Vars);
busy_expr({tuple, _Line, PM}, Vars) ->
    busy_expr(PM, Vars);
busy_expr({record, _, _, PM}, Vars) ->
    busy_expr(PM, Vars);
busy_expr({record_field, _Line, P1, P2}, Vars) ->
    busy_expr0([P1,P2], Vars);
busy_expr({match, _Line, P1, P2}, Vars) ->
    busy_expr0([P1,P2], Vars);
busy_expr({op, _, _, P1, P2}, Vars) ->
    busy_expr0([P1,P2], Vars);
busy_expr({bin_element, _, P1, P2, P3}, Vars) ->
    busy_expr0([P1,P2,P3], Vars);
busy_expr({call, _, Module, Args}, Vars) ->
    busy_expr0([Module,Args], Vars);
%%
busy_expr({var, _, '_'}, Vars) ->
    Vars;
busy_expr({var, _, Name}, Vars) ->
    dict:update_counter(Name, 1, Vars);
%%
busy_expr({cons, _, P1, P2}, Vars) ->
    busy_expr0([P1, P2], Vars);
busy_expr(_, Vars) ->
    Vars.

busy_expr0(ListExpr, VarsIn) ->
    lists:foldl(fun(E, Vars) ->
			busy_expr(E, Vars)
		end, VarsIn, ListExpr).

permute(List, {Id, Fun, Order}) ->
    permute(Order, {Fun, Id}, prepare_p(List, Id, []), []).

permute([], _, _, Result) ->
    lists:reverse(Result);
permute([P | Rest], {Fun, Id}, Dict, Result) ->
    permute(Rest, {Fun, Id + 1}, Dict,
	    [Fun(Id, dict:fetch(P, Dict)) | Result]).

prepare_p([], _, Result) ->
    dict:from_list(Result);
prepare_p([P | Rest], Id, Result) ->
    prepare_p(Rest, Id + 1, [{Id, P} | Result]).
