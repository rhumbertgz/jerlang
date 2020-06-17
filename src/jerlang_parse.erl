-module(jerlang_parse).
-export([parse_transform/2]).
-import(jerlang_misc, [parse_list/2,
		       unbounded_var/1,
		       unique_atom/1]).

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

-define(JOIN_ACTION(Line, Pattern, ResArgName,
		    CaseArgName, GuardVarName, Action, GBody),
	{'fun',Line,
               {clauses,
                [{clause,Line,
                  [{var,Line,ResArgName},{var,Line,CaseArgName}],
                  [],
                  [{match, Line, Pattern, {var,Line,ResArgName}},
                   {match, Line, {var,25, GuardVarName}, GBody},
                   {'case',Line,
                    {var,Line,CaseArgName},
                    [{clause,Line,
		      [{atom,Line,test_entry}],
		      [],
		      [{var,Line,GuardVarName}]},
                     {clause,Line,
                      [{atom,Line,run_all}],
                      [],
                      Action}]}]}]}}).

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

-define(HANDLER_MODULE, jerlang_core).
-define(HANDLER_FUNC, loop).
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

parse_transform(ASTTree, _Options) ->
    Result = parse_clause(ASTTree, []),
%    io:format("ASTTree: ~p~n", [ASTTree]),
%    io:format("Result: ~p~n", [Result]),
    Result.

parse_clause([], Ast) ->
    lists:reverse(Ast);
parse_clause([{attribute, Line, module, _}=Module | Rest], Ast) ->
    parse_clause(Rest, [?PATTERN_RECORD(pattern_joins, Line) | [ Module | Ast]]);
parse_clause([{function, L, Name, Arity, Clauses} | Rest], Ast) ->
    ParsedFunc = parse_functions(L, Name, Arity, Clauses, []),
    parse_clause(Rest, [ParsedFunc | Ast]);
parse_clause([Node | Rest], Ast) ->
    parse_clause(Rest, [Node | Ast]).

parse_functions(Line, Name, Arity, [], Result) ->
    {function, Line, Name, Arity, lists:reverse(Result)};
parse_functions(L1, Name, Arity1,
		[{clause, L2, Arity2, Guards, Body} | Rest], Result) ->
    PBody = parse_body0(Body, Arity2),
    parse_functions(L1, Name, Arity1, Rest, [{clause, L2, Arity2, Guards, PBody} | Result]).

parse_body0(Body, Arguments) ->
    {_, ArgLiveVars} = parse_body1(Arguments, sets:new(), [args]),
    {PBody, _} = parse_body1(Body, ArgLiveVars, []),
    PBody.

parse_body1(Statements, LiveVars, S) ->
    parse_body(Statements, LiveVars, [], S).

parse_body([], LiveVars, Body, _) ->
    {lists:reverse(Body), LiveVars};
parse_body([Statement | Rest], LiveVars, Body, S) ->
    {C, NLiveVars} = parse_body(Statement, LiveVars, S),
    parse_body(Rest, NLiveVars, [C | Body], S);
parse_body(Statement, LiveVars, _Body, S) ->
    parse_body(Statement, LiveVars, S).

%%  Basic clauses
parse_body({var, _Line, '_'}=Var, LiveVars, _S) ->
    {Var, LiveVars};
parse_body({var, _Line, Name} = Var, LiveVars, _S) ->
    {Var, sets:add_element(Name, LiveVars)};
parse_body({tuple, Line, PN}, LiveVars, S) ->
    {Parsed, NLiveVars} = expr_list(PN, LiveVars, S),
    {{tuple, Line, Parsed}, NLiveVars};
parse_body({nil, _Line}=Nil, LiveVars, _S) ->
    {Nil, LiveVars};
parse_body({cons, Line, P1, P2}, LiveVars, S) ->
    {Parsed1, NLiveVars1} = parse_body1(P1, LiveVars, S),
    {Parsed2, NLiveVars2} = parse_body1(P2, LiveVars, S),
    {{cons, Line, Parsed1, Parsed2},
     sets:union([NLiveVars1, NLiveVars2])};

%%  More complex constructs
parse_body({'receive', Line, Matches}, LiveVars, S) ->
    Result = parse_receive(Matches, Line, {nil, Line}, LiveVars, S),
    {Result, LiveVars};
parse_body({'receive', Line1, Matches,
	     {_, Line2, _} = Timeout, Action},
	   LiveVars, S) ->
    TimeoutOpt = ?TIMEOUT(Timeout, Action, Line2),
    P = parse_receive(Matches, Line1, TimeoutOpt, LiveVars, S),
    {P, LiveVars};
parse_body({clauses, Clauses}, LiveVars, S) ->
    {C, _NLiveVars} = parse_body1(Clauses, LiveVars, S),
    {{clauses, C}, LiveVars};
parse_body({clause, Line, Patterns, Guards, B}, LiveVars, S) ->
    {C1, NLiveVars1} = parse_body1(Patterns, LiveVars, S),
    {C2, NLiveVars2} = parse_body1(Guards, NLiveVars1, S),
    {C3, _NLiveVars3} = parse_body1(B, NLiveVars2, S),
    {{clause, Line, C1, C2, C3}, LiveVars};
parse_body({call, Line, P0, P}, LiveVars, S) ->
    {C, NLiveVars} = parse_body1(P, LiveVars, S),
    {{call, Line, P0, C}, NLiveVars};
parse_body({record, Line, Name, PM}, LiveVars, S) ->
    {PPM, NLiveVars} = expr_list(PM, LiveVars, S),
    {{record, Line, Name, PPM}, NLiveVars};
parse_body({record_field, Line, P1, P2}, LiveVars, S) ->
    {C1, LiveVars1} = parse_body1(P1, LiveVars, S),
    {C2, LiveVars2} = parse_body1(P2, LiveVars1, S),
    {{record_field, Line, C1, C2}, LiveVars2};
parse_body({match, Line, P1, P2}, LiveVars, [args]) ->
    {C1, LiveVars1} = parse_body1(P1, LiveVars, [args]),
    {C2, LiveVars2} = parse_body1(P2, LiveVars, [args]),
    {{match, Line, C1, C2}, sets:union(LiveVars1, LiveVars2)};
parse_body({match, Line, P1, P2}, LiveVars, S) ->
    {C1, LiveVars1} = parse_body1(P1, LiveVars, S),
    {C2, _LiveVars2} = parse_body1(P2, LiveVars, S),
    {{match, Line, C1, C2}, LiveVars1};
parse_body({op, Line, Op, P1, P2}, LiveVars, S) ->
    {C1, LiveVars1} = parse_body1(P1, LiveVars, S),
    {C2, LiveVars2} = parse_body1(P2, LiveVars, S),
    {{op, Line, Op, C1, C2}, sets:union(LiveVars1, LiveVars2)};
parse_body({Pattern, Line, P}, LiveVars, S) ->
    {C, _NLiveVars} = parse_body1(P, LiveVars, S),
    {{Pattern, Line, C}, LiveVars};
parse_body({Pattern, Line, P1, P2}, LiveVars, S) ->
    {C1, _NLiveVars1} = parse_body1(P1, LiveVars, S),
    {C2, _NLiveVars2} = parse_body1(P2, LiveVars, S),
    {{Pattern, Line, C1, C2}, LiveVars};
parse_body({Pattern, Line, P1, P2, P3, P4}, LiveVars, S) ->
    {C1, _NLiveVars1} = parse_body1(P1, LiveVars, S),
    {C4, _NLiveVars2} = parse_body1(P4, LiveVars, S),
    {{Pattern, Line, C1, P2, P3, C4}, LiveVars};
parse_body(Clause, LiveVars, _) ->
    {Clause, LiveVars}.

expr_list(List, LiveVars, S) ->
    lists:mapfoldl(
      fun(Clause, Acc) ->
	      {ParsedElem, NLiveVars} =
		  parse_body1(Clause, LiveVars, S),
	      {ParsedElem, sets:union(NLiveVars, Acc)}
      end, LiveVars, List).
	      


%%  -------   Specific parsing  ---------

parse_receive(Matches, Line, TimeoutOpt, LiveVars, S) ->
    parse_receive(Matches, Line, 1, TimeoutOpt, LiveVars, S, {[], []}).

parse_receive([], Line, _Id, TimeoutOpt, _LiveVars, _S, {Records, Functions}) ->
    FirstArg = parse_list(lists:reverse(Records), Line),
    SecondArg = parse_list(lists:reverse(Functions), Line),
    {call, Line,
     {remote, Line, {atom, Line, ?HANDLER_MODULE}, {atom, Line, ?HANDLER_FUNC}},
     [FirstArg, SecondArg, TimeoutOpt]};
parse_receive([{clause, LineC, Patterns, Guards, Action} | Rest],
	      Line, Id, TimeoutOpt, LiveVars, S, {Records, Functions}) ->
    {_, NLiveVars1} = parse_body1(Patterns, LiveVars, S),
    %%  FIXME: is the one below actually needed?
    {_, NLiveVars2} = parse_body1(Guards, NLiveVars1, S),
    {PAction, _NLiveVars3} = parse_body1(Action, NLiveVars2, S),
    {PPatterns, PPatternsRec, NId} =
	parse_patterns(Patterns, LineC, Id, LiveVars),
    ResUniqueName = unique_atom(?HANDLER_RES_ARG_BASE),
    CaseUniqueName = unique_atom(?HANDLER_CASE_ARG_BASE),
    GuardsVar = unique_atom(?HANDLER_GUARD_BASE),

    PGuardsBody
	= receive_guards(Guards, Line),
    ActGuardsFunc = ?JOIN_ACTION(Line, PPatterns, ResUniqueName,
				 CaseUniqueName, GuardsVar,
				 PAction, PGuardsBody),
    parse_receive(Rest, Line, NId, TimeoutOpt,
		  LiveVars, S,
		  {[PPatternsRec | Records],
		   [ActGuardsFunc | Functions]}).

parse_patterns(Patterns, Line, Id, LiveVars) ->
    parse_patterns(Patterns, Line, Id, {[], []}, LiveVars, true).

parse_patterns([], Line, Id, {Patterns, PatternsRecords}, _LiveVars, _) ->
    {parse_list(lists:reverse(Patterns), Line),
     parse_list(lists:reverse(PatternsRecords), Line),
     Id};
parse_patterns([Pattern | Rest], Line, Id,
	       {RetPattern0, RetPatternRec0}, LiveVars, Decode) ->
    {PPattern, Rec, NId} =
	parse_patterns(Pattern, Line, Id,
		       {[], []}, LiveVars, Decode),
    parse_patterns(Rest, Line, NId,
		   { PPattern ++ RetPattern0, Rec ++ RetPatternRec0},
		   LiveVars, true);

%%  Match is 'wrongly' recognized by the standard compiler
%%  And it brakes things horribly. We need to manually
%%  find joins and pack them together
%%  Later we reply the function again but we know that patterns
%%  are now ok, so we omit this function
parse_patterns({match, Line0, _One, _Two}=Match, Line1,
	       Id, {RetPattern, RetPatternRec},
	       LiveVars, Decode) when (Decode == true)->
    Decoded = decode_match(Match),
    JoinsEncoded = join_patterns(Decoded, Line0),
    parse_patterns(JoinsEncoded, Line1, Id,
		   {RetPattern, RetPatternRec},
		   LiveVars, false);
parse_patterns({op, Line, 'and', LPattern, RPattern},
	       _L, Id, {RetPattern,RetPatternRec},
	       LiveVars, Decode) ->
    {PLPattern, PLPatternRec, Id2} =
	parse_patterns(LPattern, Line, Id,
		       {[], []}, LiveVars, Decode),
    {PRPattern, PRPatternRec, Id3} = 
	parse_patterns(RPattern, Line, Id2,
		       {[], []}, LiveVars, Decode),
    {PRPattern ++ PLPattern ++ RetPattern,
     PRPatternRec ++ PLPatternRec ++ RetPatternRec,
     Id3};
parse_patterns(Other, Line, Id, _, LiveVars, _) ->
    parse_pattern(Other, Line, Id, LiveVars).

parse_pattern({call, _Line1, {atom, _Line2, prop}, [Pattern]},
	     Line, Id, LiveVars) ->
    {_PPattern, _Rec, _NId} =
	parse_pattern(Pattern, Line, Id, LiveVars, yes);
parse_pattern(Pattern, Line, Id, LiveVars) ->
    {_PPattern, _Rec, _NId} =
	parse_pattern(Pattern, Line, Id, LiveVars, no).

parse_pattern(Pattern, Line, Id, LiveVars, Propagate) ->
    {NPattern, Case} = unbound_pattern(Pattern, LiveVars, false),
    CaseTests = case_test_clauses(NPattern, Line, Case),
    UniqueVar = unique_atom(?HANDLER_P_ARG_BASE),
    Rec = ?LOCAL_PATTERN(Id, Line, UniqueVar, CaseTests, Propagate),
    {[Pattern], [Rec], Id + 1}.

unbound_pattern({match, Line, P1, P2}, LiveVars, Case) ->
    {R1, Case1} = unbound_pattern(P1, LiveVars, Case),
    {R2, Case2} = unbound_pattern(P2, LiveVars, Case),
    {{match, Line, R1, R2}, (Case1 or Case2)};
unbound_pattern({tuple, Line, PN}, LiveVars, _Case) ->
    RN = lists:map(
	       fun(Elem) ->
		       {R, _} = unbound_pattern(Elem, LiveVars, true),
		       R
	       end, PN),
    {{tuple, Line, RN}, true};
unbound_pattern({cons, Line, P1, P2}, LiveVars, Case) ->
    {R1, _} = unbound_pattern(P1, LiveVars, Case),
    {R2, _} = unbound_pattern(P2, LiveVars, Case),
    {{cons, Line, R1, R2}, true};
unbound_pattern({var, Line, Name} = Var, LiveVars, Case) ->
    case sets:is_element(Name, LiveVars) of
	true -> {Var, true};
	false -> {{var, Line, unbounded_var(Name)}, Case}
    end;
unbound_pattern({record, Line, Name, PN}, LiveVars, _Case) ->
    RN = lists:map(
	   fun(Elem) ->
		   {R, _} = unbound_pattern(Elem, LiveVars, true),
		   R
	   end, PN),
    {{record, Line, Name, RN}, true};
unbound_pattern({record_field, Line, P1, P2}, LiveVars, _Case) ->
    {R1, _} = unbound_pattern(P1, LiveVars, true),
    {R2, _} = unbound_pattern(P2, LiveVars, true),
    {{record_field, Line, R1, R2}, true};
unbound_pattern({bin, Line, PN}, LiveVars, _Case) ->
    RN = lists:map(
	   fun(Elem) ->
		   {R, _} = unbound_pattern(Elem, LiveVars, true),
		   R
	   end, PN),
    {{bin, Line, RN}, true};
unbound_pattern({bin_element, Line, P1, P2, P3}, LiveVars, _Case) ->
    {R1, _} = unbound_pattern(P1, LiveVars, true),
    %% P2 is the size of the bits that we take
    %% and cannot be unbounded
    %% P3 is the type of the bits we take
    %% and cannot be a variable
    {_, Case2} = unbound_pattern(P2, LiveVars, false),
    {_, Case3} = unbound_pattern(P3, LiveVars, false),
    R2 = case Case2 of
	     true -> P2;
	     false -> default
	 end,
    R3 = case Case3 of
	     true -> P3;
	     false -> default
	 end,
    {{bin_element, Line, R1, R2, R3}, true};
unbound_pattern(P, _LiveVars, _Case) ->    
    {P, true}.

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

receive_guards([], Line) ->
    {atom, Line, true};
receive_guards([[Guards]], _) ->
    Guards.

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
