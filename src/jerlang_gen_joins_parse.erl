-module(jerlang_gen_joins_parse).
-export([parse_transform/2]).

-import(jerlang_misc, [parse_list/2,
		       unbounded_var/1,
		       is_unbounded_var/1,
		       unique_atom/1,
		       unique_vars/2,
		       busy_expr/2,
		       permute/2]).

-define(LOCAL_PATTERN(Id, Line, UniqueVar, CaseTests, Propagation),
	{tuple, Line,[
	{integer,Line,Id},
	{record,Line,pattern_joins,
	 [{record_field,Line,
	   {atom,Line,test},
	   {'fun',Line,
	    {clauses,
	     [{clause,Line,
	       [{var,Line,UniqueVar}],
	       [],
	       [{'case',Line,
		 {var,Line,UniqueVar},
		 CaseTests
		}]
	      }]
	    }}},
	  {record_field,Line,
	   {atom,Line,msgs},
	   {nil,Line}},
	  {record_field,Line,
	   {atom,Line,prop},
	   {atom,Line,Propagation}}
	 ]}]}).

-define(JOIN_GUARDS_TEST(Line, Join, State, Guards),
	{tuple, Line,
	 [{'fun',Line,
	   {clauses,
	    [{clause,Line,
	      [Join, State],
	      Guards,
	      [{atom, Line, true}]}]}},
	  {nil, Line}]}).

-define(TIMEOUT(Time, Action, Line),
	{cons,Line,
	 {tuple,Line,
	  [{atom,Line,timeout},
	   Time,
	   {'fun',Line,
	    {clauses,
	     [{clause,Line,[],[],
	       Action}]}}]},
	 {nil,Line}}).

-define(JOINS_INIT(Line, Tests),
	{function, Line, init_joins, 1,
	 [{clause, Line,
	   [{var, Line, '_'}],
	   [],
	   [{tuple, Line,
	     [{atom, Line, ok}, Tests]}]}]}).
-define(ORDERING(Initial, Line, Vars, StatusName, NewVars),
	[{match, Line,
	  {tuple, Line, Vars ++ [{var, Line, StatusName}]},
	  Initial},
	 {tuple, Line, NewVars ++ [{var, Line, StatusName}]}]).


-define(HANDLER_RES_ARG_BASE, "ResArgName").
-define(HANDLER_CASE_ARG_BASE, "CaseArg").
-define(HANDLER_GUARD_BASE, "Guard").
-define(HANDLER_P_ARG_BASE, "Value").
-define(PATTERN_RECORD_NAME, pattern_joins).

-define(PATTERN_RECORD(PatternName, Line),
	{attribute,Line,record,
           {PatternName,
            [{record_field,Line,{atom,Line,test}},
             {record_field,Line,{atom,Line,msgs},{nil,Line}},
             {record_field,Line,{atom,Line,prop},{atom,Line,no}}]}}).

-define(JOINS_EXPORT(Line),
	{attribute, Line, export,
	 [{init_joins, 1}]}).

-define(JOINS_DICT_NAME, init_joins_dict).

parse_transform(ASTTree, _Options) ->
    
    Result = parse_clause(ASTTree, []),
%    io:format("ASTTree: ~p~n", [ASTTree]),
%    ASTTree.
%    io:format("Result: ~p~n", [Result]),
    Result.

parse_clause([], [{eof, LEnd} | Ast]) ->
    %% Add the collected join information
    Init = case get(?JOINS_DICT_NAME) of
	       undefined -> [{eof, LEnd}];
	       Other     -> [{eof, LEnd}, Other]
	   end,
    Tree = Init ++ Ast,
    lists:reverse(Tree);
parse_clause([{attribute, Line, behaviour, jerlang_gen_joins}=Behaviour | Rest], Ast) ->
    parse_clause(Rest, [?JOINS_EXPORT(Line+1), Behaviour | Ast]);
parse_clause([{attribute, Line, module, _}=Module | Rest], Ast) ->
    parse_clause(Rest, [?PATTERN_RECORD(pattern_joins, Line) | [ Module | Ast]]);
parse_clause([{function, L, handle_join, 2, Clauses} | Rest], Ast) ->
    {ParsedClauses, ParsedTests} = parse_receive(Clauses, L),
    put(?JOINS_DICT_NAME,
	?JOINS_INIT(L-1, ParsedTests)),
    parse_clause(Rest,
		 [{function, L, handle_join, 2, ParsedClauses} | Ast]);
parse_clause([{function, _, handle_join, _, _} | _], _) ->
    exit({error, invalid_handle_join_arity});
parse_clause([Node | Rest], Ast) ->
    parse_clause(Rest, [Node | Ast]).

%%  -------   Specific parsing  for handle_join function ---------
parse_receive(Clauses, Line) ->
    parse_receive(Clauses, Line, 1, {[], []}).

parse_receive([], Line, _Id, {Joins, Tests}) ->
    {lists:reverse(Joins),
     parse_list(lists:reverse(Tests), Line)};
parse_receive([{clause, LineC, [Join, State], Guards, Action}| Rest],
	      Line, Id, {Joins, JoinsTests}) ->
    {RawPatterns0, PTests0, PVars, NId} = 
	parse_patterns(Join, LineC, Id, {[], []}, dict:new(), true),
    RawPatterns = lists:reverse(RawPatterns0),
    PTests = lists:reverse(PTests0),

    FOrder = get_ordering(RawPatterns, Id, {State, Guards}),

    {FPatterns,
     FPTests,
     FAction} =
	case FOrder of
		    valid ->
			{RawPatterns, PTests, Action};
		    _ ->
			Identity = fun(_, Q) -> Q end,
			OrderTest =
			    fun(Id0, {tuple, L0,
				      [{integer, L1, _} | R0]}) ->
				    {tuple, L0,
				     [{integer, L1, Id0} | R0]}
			    end,
			PPatterns =
			    permute(RawPatterns,
				    {Id, Identity, FOrder}),
			PPTests =
			    permute(PTests,
				    {Id, OrderTest, FOrder}),
			PAction =
			    permute_action(Action, Id, FOrder),
			{PPatterns, PPTests, PAction}
		end,

    BetaFunctions =
	gen_beta_functions(
	  {FPatterns, State,
	   Guards, LineC}, PVars),

    JoinResult =
	parse_list(
	  [FPTests | BetaFunctions],
	  LineC),
    
    C1 = {clause, LineC,
	  [parse_list(FPatterns, LineC), State],
	  Guards, FAction},
    parse_receive(Rest, Line, NId,
		  {[C1 | Joins],
		   [JoinResult | JoinsTests]}).

%% Ordering is disabled by default because it can
%% do unpredictable things for the user
-ifdef(use_ordering_opt).
get_ordering(RawPatterns, Id, {State, Guards}) ->
    {ok, Forder} = 
	jerlang_gen_joins_order:optimize(RawPatterns, Id,
					 {State, Guards}),
    Forder.

-else.
get_ordering(_, _, _) ->
    valid.
-endif.


gen_beta_functions({Patterns, State, Guards, LineC}, Vars0) ->
    Vars1 = busy_expr(State, Vars0),
    Vars2 = busy_expr(Guards, Vars1),
    NPatterns =
	lists:map(
	  fun(Elem) ->
		  {RPattern, _} = unbound_pattern(Elem, Vars2, true),
		  RPattern
	  end, Patterns),
    SVars = busy_expr(State, dict:new()),
    GVars = busy_expr(Guards, dict:new()),

    {NState, _} = unbound_pattern(State, Vars2, true),
    NGuards = guards_cond(Guards, GVars, Vars1),
    
    gen_beta_functions1({NPatterns, [], NState, NGuards, LineC},
			{Vars2, vars_split(SVars, GVars)},
			{false, false}, []).

%%  FIXME: need to analyze also the guard expressions.
%%  Currently we assume they are ok (i.e. contain no free,
%%  variables).
%%  FIXME: currently Cond flags are not used but
%%  we could improve the perfomance by having
%%  the state/guards only once (earliest) and not
%%  whenever they are available.
gen_beta_functions1({[], _, _, _, _}, _, _, _) ->
    exit({error, invalid_handle_join_definition});
%%  Special case for *only* two patterns in the join.
%%  Treat it only as a single beta reduction.
%%  For efficiency. There is no point in dividing this
%%  into subproblems.
gen_beta_functions1({[P1, P2 | Rest], [], State, Guards, LineC},
		    Vars, Cond, []) ->
    gen_beta_functions1({[P2 | Rest], [P1], State, Guards, LineC},
			Vars, Cond, []);
%%  Have more cases for {false, true}, etc...?
gen_beta_functions1({[Pattern], PrevP, State, Guards, LineC},
		    {VarsAll, _},
		    _, Res) ->
    {NState, _} = unbound_pattern(State, VarsAll, true),
    TransfJoin = parse_list(
		   lists:reverse(
		     [Pattern | PrevP]), LineC),
    BetaTest = ?JOIN_GUARDS_TEST(
		  LineC, TransfJoin,
		  NState, Guards),
    lists:reverse([BetaTest | Res]);
gen_beta_functions1(
  {[Pattern | Rest], PrevP, State, Guards, LineC},
  {_, {Common, SPVars, GPVars}}=Vars,
  _, Res) ->

    %%  Find out whether state and/or can be included
    %%  depending on the existence of the variables
    %%  in the Patterns
    Patterns = [Pattern | PrevP],
    PVarsDict = busy_expr(Patterns, dict:new()),
    PVars = dict:fetch_keys(PVarsDict),

    {BetaGuards, GuardsRes} =
	guards_avail({length(GPVars -- PVars) == 0,
		      Guards, []}),

    BetaState =
	state_avail(Common, GuardsRes,
		    {SPVars -- PVars, State,
		     fun jerlang_misc:unbounded_var_null/1}),
    
    NPatterns =
	p_avail(Common, {GPVars, GuardsRes},
		SPVars, PVarsDict, Patterns),
    
    BetaTest = ?JOIN_GUARDS_TEST(LineC,
				 parse_list(
				   lists:reverse(NPatterns),
				   LineC),
				 BetaState, BetaGuards),

    gen_beta_functions1(
      {Rest, [Pattern | PrevP], State, Guards, LineC},
      Vars, {false, false}, [BetaTest | Res]).

%% Ordering functions. Correctly change the targets
%% for replies
permute_action(Action, _, [_]) ->
    Action;
permute_action(Action, Id, Order) ->
    %% FIXME: should detect the situation when number
    %% of elements at the end does not match header patterns
    %% Currently it may throw a warning "no clause will match"
    {Last, ActionRest} = partition_act(Action, []),
    Line = 
	case Last of
	    [] ->
		0;
	    _ ->
		element(2, Last)
	end,	    
    UniqueVar = unique_atom("OrderAct"),
    UniqueVars =
	lists:map(fun(Name) -> {var, Line, Name} end,
		  unique_vars(UniqueVar, length(Order))),

    ResultVars =
	permute(UniqueVars,
		{Id, fun(_, Q) -> Q end, Order}),
    StatusName = unique_atom("StatusNew"),
    NLast = 
	?ORDERING(Last, Line, [parse_list(UniqueVars, Line)],
		  StatusName, [parse_list(ResultVars, Line)]),
    ActionRest ++ NLast.
    
partition_act([], _) ->
    {[], []};
partition_act([Last], Result) ->
    {Last, lists:reverse(Result)};
partition_act([E | Rest], Result) ->
    partition_act(Rest, [E | Result]).
    
%%  -------------------------------------------------
%%  Returns information collected during the parsing of
%%  Mod:handle_join arguments.
%%  {standard join represented as list of patterns,
%%   list of tests for each pattern,
%%   frequency of the variables *within* the header,
%%   ID for the next join-pattern}
%%  -------------------------------------------------

%%  Match is 'wrongly' recognized by the standard compiler
%%  And it brakes things horribly. We need to manually
%%  find joins and pack them together
%%  Later we reply the function again but we know that patterns
%%  are now ok, so we omit this function
parse_patterns({match, Line0, _One, _Two}=Match, Line1,
	       Id, {RetPattern, RetPatternRec},
	       Vars, true)->
    Decoded = decode_match(Match),
    JoinsEncoded = join_patterns(Decoded, Line0),
    parse_patterns(JoinsEncoded, Line1, Id,
		   {RetPattern, RetPatternRec},
		   Vars, false);
parse_patterns({op, Line, 'and', LPattern, RPattern},
	       _L, Id, {RetPattern,RetPatternRec},
	       Vars, Decode) ->
    {PLPattern, PLPatternRec, Vars2, Id2} =
	parse_patterns(LPattern, Line, Id,
		       {[], []}, Vars, Decode),
    {PRPattern, PRPatternRec, Vars3, Id3} = 
	parse_patterns(RPattern, Line, Id2,
		       {[], []}, Vars2, Decode),
    {PRPattern ++ PLPattern ++ RetPattern,
     PRPatternRec ++ PLPatternRec ++ RetPatternRec,
     Vars3, Id3};

parse_patterns(Other, Line, Id, _, Vars, _) ->
    parse_pattern(Other, Line, Id, Vars).

parse_pattern({call, _Line1, {atom, _Line2, prop}, [Pattern]},
	     Line, Id, Vars) ->
    {_PPattern, _Rec, _NVars, _NId} =
	parse_pattern(Pattern, Line, Id, Vars, yes);
parse_pattern(Pattern, Line, Id, Vars) ->
    {_PPattern, _Rec, _NVars, _NId} =
	parse_pattern(Pattern, Line, Id, Vars, no).

parse_pattern(Pattern, Line, Id, Vars, Propagate) ->
    {NPattern, Case} = unbound_pattern(Pattern, dict:new(), false),
    CaseTests = case_test_clauses(NPattern, Line, Case),
    UniqueVar = unique_atom(?HANDLER_P_ARG_BASE),
    Rec = ?LOCAL_PATTERN(Id, Line, UniqueVar, CaseTests, Propagate),
    NVars = busy_expr(Pattern, Vars),
    {[Pattern], [Rec], NVars, Id + 1}.

%%  -----------------------------------------------------
unbound_pattern({match, Line, P1, P2}, Vars, Case) ->
    {R1, Case1} = unbound_pattern(P1, Vars, Case),
    {R2, Case2} = unbound_pattern(P2, Vars, Case),
    {{match, Line, R1, R2}, (Case1 or Case2)};
unbound_pattern({tuple, Line, PN}, Vars, _Case) ->
    RN = lists:map(
	       fun(Elem) ->
		       {R, _} = unbound_pattern(Elem, Vars, true),
		       R
	       end, PN),
    {{tuple, Line, RN}, true};
unbound_pattern({cons, Line, P1, P2}, Vars, Case) ->
    {R1, _} = unbound_pattern(P1, Vars, Case),
    {R2, _} = unbound_pattern(P2, Vars, Case),
    {{cons, Line, R1, R2}, true};
unbound_pattern({op, Line, Op, P1, P2}, Vars, Case) ->
    {R1, _} = unbound_pattern(P1, Vars, Case),
    {R2, _} = unbound_pattern(P2, Vars, Case),
    {{op, Line, Op, R1, R2}, true};
unbound_pattern({var, Line, Name} = Var, Vars, Case) ->
    case dict:find(Name, Vars) of
	{ok, Number} when (Number > 1) ->
	    {Var, true};
	_ ->
	    {{var, Line, unbounded_var(Name)}, Case}
    end;
unbound_pattern({record, Line, Name, PN}, Vars, _Case) ->
    RN = lists:map(
	   fun(Elem) ->
		   {R, _} = unbound_pattern(Elem, Vars, true),
		   R
	   end, PN),
    {{record, Line, Name, RN}, true};
unbound_pattern({record_field, Line, P1, P2}, Vars, _Case) ->
    {R1, _} = unbound_pattern(P1, Vars, true),
    {R2, _} = unbound_pattern(P2, Vars, true),
    {{record_field, Line, R1, R2}, true};
unbound_pattern({bin, Line, PN}, Vars, _Case) ->
    RN = lists:map(
	   fun(Elem) ->
		   {R, _} = unbound_pattern(Elem, Vars, true),
		   R
	   end, PN),
    {{bin, Line, RN}, true};
unbound_pattern({bin_element, Line, P1, P2, P3}, Vars, _Case) ->
    {R1, _} = unbound_pattern(P1, Vars, true),
    %% P2 is the size of the bits that we take
    %% and cannot be unbounded
    %% P3 is the type of the bits we take
    %% and cannot be a variable
    {_, Case2} = unbound_pattern(P2, Vars, false),
    {_, Case3} = unbound_pattern(P3, Vars, false),
    R2 = case Case2 of
	     true -> P2;
	     false -> default
	 end,
    R3 = case Case3 of
	     true -> P3;
	     false -> default
	 end,
    {{bin_element, Line, R1, R2, R3}, true};
unbound_pattern([ListExpr], Vars0, Case0) ->
    {R, _} = unbound_pattern(ListExpr, Vars0, Case0),
    {[R], true};
unbound_pattern(P, _Vars, _Case) ->    
    {P, true}.


term_replace({var, Line, Name}=Var, V, N) ->
    case lists:member(Name, V) of
	true -> N(Line);
	_    -> Var
    end;
term_replace({Op, Line, E}, V, N) ->
    {Op, Line, term_replace(E, V, N)};
term_replace({Op, Line, E1, E2}, V, N) ->
    {Op, Line,
     term_replace(E1, V, N),
     term_replace(E2, V, N)};
term_replace({Op, Line, E1, E2, E3}, V, N) ->
    {Op, Line,
     term_replace(E1, V, N),
     term_replace(E2, V, N),
     term_replace(E3, V, N)};
term_replace(E, V, N) when is_list(E) ->
    lists:map(fun(Elem) -> term_replace(Elem, V, N) end, E);
term_replace(E, _, _) ->
    E.

case_test_clauses(Pattern, Line, false) ->
    [{clause, Line,
      [Pattern],
      [],
      [{atom, Line, true}]}];
case_test_clauses(Pattern, Line, true) ->
    [{clause, Line,
      [Pattern],
      [],
      [{atom, Line, true}]},
     {clause, Line,
      [{var, Line, '_'}],
      [],
      [{atom, Line, false}]}].

%%  ------------------------------------------------

%% Functions responsible for transforming
%% match to acceptable pattern structure
decode_match({match, Line0, Left0, {match, _Line, _Left1, _Right1}=Right0}) ->
    %%  Since the code is fine we are sure that first
    %%  element is definitely not the match itself
    %% But it can be an 'and' op.

    {Pattern, Rest0} = match_r_most(Left0),
    [Head | Rest2] = decode_match(Right0),
    {MatchedPattern, Rest1} = match_l_most(Head),
    Rest0 ++ [{match, Line0, Pattern, MatchedPattern} | Rest1] ++ Rest2;
decode_match({match, Line0, Left0, {op, _, 'and', _, _}=Right0}) ->
    {Pattern, Rest0} = match_r_most(Left0),
    {MatchedPattern, Rest1} = match_l_most(Right0),
    Rest0 ++ [{match, Line0, Pattern, MatchedPattern} | Rest1];
decode_match({match, _Line0, _, {op, _, _Other, _, _}}) ->
    exit({error, invalid_join_operator});
decode_match({match, Line0, Left0, Right0}) ->
    {Pattern, Rest} = match_r_most(Left0),
    Rest ++ [{match, Line0, Pattern, Right0}].

match_r_most({op, _, 'and', Left0, Right0}) ->
    {Pattern, RestJoins} = match_r_most(Right0),
    {Pattern, [Left0 | RestJoins]};
match_r_most({op, _, Other, _, _}) ->
    exit({error, {invalid_join_operator, Other}});
match_r_most(Pattern) ->
    {Pattern, []}.

match_l_most({match, _, _, _}) ->
    exit({error, invalid_match_l_most});
match_l_most({op, _, 'and', Left0, Right0}) ->
    {Pattern, RestJoins} = match_l_most(Left0),
    {Pattern,  RestJoins ++ [Right0]};
match_l_most({op, _, Other, _, _}) ->
    exit({error, {invalid_join_operator, Other}});
match_l_most(Pattern) ->
    {Pattern, []}.

join_patterns(Patterns, Line) ->
    join_patterns1(lists:reverse(Patterns), Line).

join_patterns1([], _) ->
    [];
join_patterns1([Elem], _) ->
    Elem;
join_patterns1([Right, Left], Line) ->
    {op, Line, 'and', Left, Right};
join_patterns1([Head | Rest], Line) ->
    Result = join_patterns1(Rest, Line),
    {op, Line, 'and', Result, Head}.


guards_avail({false, _, Null}) ->
    {Null, false};
guards_avail({true, Target, _}) ->
    {Target, true}.

state_avail(C, false,
	    {SVars, State, Null}) ->
    term_replace(State, C ++ SVars, Null);
state_avail(_, true,
	    {SVars, State, Null}) ->
    term_replace(State, SVars, Null).

p_avail(_, {_, false},
	SVars, PVarsDict, P) ->
    F0 = 
	dict:fetch_keys(
	  dict:filter(
	    fun(_, 1) ->
		    true;
	       (_, _) ->
		    false
	    end,PVarsDict)),
    F1 = F0 -- SVars,
    term_replace(P, F1, fun jerlang_misc:unbounded_var_null/1);
p_avail(Common, {GVars, true},
        SVars, PVarsDict, P) ->
    F0 = 
	dict:fetch_keys(
	  dict:filter(
	    fun(_, 1) ->
		    true;
	       (_, _) ->
		    false
	    end,PVarsDict)),
    F1 = F0 -- Common,
    F2 = F1 -- GVars,
    F3 = F2 -- SVars,
    term_replace(P, F3, fun jerlang_misc:unbounded_var_null/1).
	    
vars_split(Dic1, Dic2) ->
    Dic0 = dict:merge(
	     fun(_, V1, V2) -> V1 + V2 end,
	     Dic1, Dic2),
    %% Variables that are shared between
    %% state and guards
    Common = dict:fetch_keys(
	      dict:filter(
		fun(_, 1) ->
			false;
		   (_, _) ->
			true
		end, Dic0)),
    Diff = (dict:fetch_keys(Dic0) -- Common),
    {SPVars, GPVars} = lists:partition(
			 fun(Key) ->
				 dict:is_key(Key, Dic1)
			 end, Diff),
    {Common, SPVars, GPVars}.

%%  If guard contains a free variables then
%%  replace it with a neutral atom because
%%  original guard clause will complain anyway.
guards_cond(Val, GVars, AllVars) ->
    guards_cond0(Val, dict:fetch_keys(GVars) --
		dict:fetch_keys(AllVars)).

guards_cond0(Guards, []) ->
    Guards;
guards_cond0(Guards, Vars) ->
    term_replace(Guards, Vars,
		fun(Line) ->
		       {atom, Line, true}
		end).
