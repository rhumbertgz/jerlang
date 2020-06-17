-module(jerlang_gen_joins).

-export([start/3, start/4, start_link/3, start_link/4]).
-export([call/2, call/3, cast/2]).

%% Internal exports
-export([init_it/6, behaviour_info/1]).

-import(error_logger, [format/2]).
-import(jerlang_gen_joins_mset, [first/3, beta/3]).
-include("jerlang.hrl").
-define(GEN_JOINS_MAILBOX, gen_joins_mailbox).
-define(GEN_JOINS_DUPLICATES, gen_joins_duplicates).

% -spec (behaviour_info/1 :: (atom()) -> 'undefined' | behaviour()).
-spec behaviour_info(atom()) -> 'undefined' | behaviour().
-type ref() :: reference().

behaviour_info(callbacks) ->
    [{init, 1},
     {init_joins, 1},
     {handle_join, 2},
     {terminate, 0}];

behaviour_info(_Other) ->
    undefined.

% Start the joins server
start(Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Mod, Args, Options).

start(Name, Mod, Args, Options) ->
    gen:start(?MODULE, nolink, Name, Mod, Args, Options).

start_link(Mod, Args, Options) ->
    gen:start(?MODULE, link, Mod, Args, Options).

start_link(Name, Mod, Args, Options) ->
    gen:start(?MODULE, link, Name, Mod, Args, Options).

%% -----------------------------------------------------------------
%% Make a call to a generic server.
%% If the server is located at another node, that node will
%% be monitored.
%% If the client is trapping exits and is linked server termination
%% is handled here (? Shall we do that here (or rely on timeouts) ?).
%% ----------------------------------------------------------------- 
call(Name, Request) ->
    case catch gen:call(Name, '$gen_call', Request) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request]}})
    end.

call(Name, Request, Timeout) ->
    case catch gen:call(Name, '$gen_call', Request, Timeout) of
	{ok,Res} ->
	    Res;
	{'EXIT',Reason} ->
	    exit({Reason, {?MODULE, call, [Name, Request, Timeout]}})
    end.

%% -----------------------------------------------------------------
%% Make a cast to a generic server.
%% -----------------------------------------------------------------
cast({global,Name}, Request) ->
    catch global:send(Name, cast_msg(Request)),
    ok;
cast({Name,Node}=Dest, Request) when is_atom(Name), is_atom(Node) -> 
    do_cast(Dest, Request);
cast(Dest, Request) when is_atom(Dest) ->
    do_cast(Dest, Request);
cast(Dest, Request) when is_pid(Dest) ->
    do_cast(Dest, Request).

do_cast(Dest, Request) -> 
    do_send(Dest, cast_msg(Request)),
    ok.

cast_msg(Request) -> {'$gen_cast',Request}.

%%%========================================================================
%%% Gen-callback functions
%%%========================================================================

%%% ---------------------------------------------------
%%% Initiate the new process.
%%% Register the name using the Rfunc function
%%% Calls the Mod:init/Args function.
%%% Finally an acknowledge is sent to Parent and the main
%%% loop is entered.
%%% ---------------------------------------------------
%%% First gather information about any joins from the Mod:init_joins
%%% function and parse it.
%%% Call to Mod:init sets the initial value for the
%%% Mod:handle_join functions.
%%% ---------------------------------------------------
init_it(Starter, self, Name, Mod, Args, Options) ->
    init_it(Starter, self(), Name, Mod, Args, Options);
init_it(Starter, Parent, Name, Mod, Args, Options) ->
    Debug = debug_options(Name, Options),
    Joins =
	case catch verify_join(Mod:init_joins(none)) of
	    {ok, JoinsDef} ->
		JoinsDef;
	    Other ->
		%io:format("Errror: ~p~n", [Other]),
		proc_lib:init_ack(Starter, {error, Other}),
		exit({error, Other})
	end,
    case catch Mod:init(Args) of
	{ok, State} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, infinity, Joins, Debug);
	{ok, State, Timeout} ->
	    proc_lib:init_ack(Starter, {ok, self()}), 	    
	    loop(Parent, Name, State, Mod, Timeout, Joins, Debug);
	{stop, Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	ignore ->
	    proc_lib:init_ack(Starter, ignore),
	    exit(normal);
	{'EXIT', Reason} ->
	    proc_lib:init_ack(Starter, {error, Reason}),
	    exit(Reason);
	Else ->
	    Error = {bad_return_value, Else},
	    proc_lib:init_ack(Starter, {error, Error}),
	    exit(Error)
    end.

%%%========================================================================
%%% Internal functions
%%%========================================================================
%%% ---------------------------------------------------
%%% The MAIN loop.
%%% ---------------------------------------------------
loop(Parent, Name, State, Mod, hibernate, Joins, Debug) ->
    proc_lib:hibernate(?MODULE,wake_hib,
		       [Parent, Name, State, Mod, Joins, Debug]);
loop(Parent, Name, State, Mod, Time, Joins, Debug) ->
    Msg = receive
	      Input ->
		    Input
	  after Time ->
		  timeout
	  end,
    decode_msg(Msg, Parent, Name, State, Mod, Time, Joins, Debug, false).

%% FIXME: allow for hibernation. Compare with R12 release
%wake_hib(Parent, Name, State, Mod, Joins, Debug) ->
%    Msg = receive
%	      Input ->
%		  Input
%	  end,
%    decode_msg(Msg, Parent, Name, State, Mod, hibernate, Joins, Debug, true).

decode_msg(Msg, Parent, Name, State, Mod, Time, Joins, Debug, Hib) ->
    case Msg of
	{system, From, Req} ->
	    sys:handle_system_msg(Req, From, Parent, ?MODULE, Debug,
				  [Name, State, Mod, Time, Joins], Hib);
	{'EXIT', Parent, Reason} ->
	    terminate(Reason, Name, Msg, Mod, State, Joins, Debug);
	{internal, joins} ->
	    handle_msg1(none, Parent, Name, State, Mod, Time, Joins, Debug);
	_Msg when Debug =:= [] ->
	    handle_msg0(Msg, Parent, Name, State, Mod, Time, Joins, Debug);
	_Msg ->
	    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, 
				      Name, {in, Msg}),
	    handle_msg0(Msg, Parent, Name, State, Mod, Time, Joins, Debug1)
    end.

%check_size() ->
%    {ok, Mailbox} = gen_joins_mailbox(),
%    Size = jerlang_dict:size(Mailbox),
%    case (Size > 200) and ( (Size rem 10) == 0) of
%	true ->
%	    io:format("Size is bigger ~p~n", [Size]);
%	_ ->
%	    ok
%    end.

handle_msg0({'$gen_call', _, _}=Call, Parent, Name, State,
	    Mod, Time, Joins, Debug) ->
    handle_msg1(Call, Parent, Name, State,
		Mod, Time, Joins, Debug);
%% Adapting cast to common pattern
handle_msg0({'$gen_cast', Msg}, Parent, Name, State,
	    Mod, Time, Joins, Debug) ->
    handle_msg1({'$gen_cast', none, Msg}, Parent, Name, State,
		Mod, Time, Joins, Debug);
handle_msg0(_Other, _Parent, _Name, _State,
	    _Mod, _Time, _Joins, _Debug) ->
    %% FIXME: possibly just ignore?
    exit({error, unrecognized_message}).

handle_msg1(Msg, Parent, Name, State,
	    Mod, Time, Joins, Debug) ->
    %io:format("Handle message~n", []),
    %% Find any matching join
    JoinsMatching = process_join(Msg, Joins, State),
    %% Analyse result of matching
    case JoinsMatching of
	{error, NJoins} ->
	    loop(Parent, Name, State, Mod, Time, NJoins, Debug);
	{ok, {Msgs, NJoins}} ->
	    Split = lists:map(
		       fun({Tag, From, Value}) ->
			       {{Tag, From}, Value}
		       end, Msgs),
	    {Tags, Values} = lists:unzip(Split),
%	    io:format("Handling join ~p~n", [Values]),
	    JoinCall =
		try Mod:handle_join(Values, State)
		catch
		    exit:Exc -> {exit, Exc};
		    error:Exc -> {exit, Exc}
		end,
	    handle_join_result(JoinCall, Tags, Parent,
			       Name, State, Mod,
			       Time, NJoins, Debug)
    end.

handle_join_result({stop, Reason}, MsgTags, _Parent, Name, _OldsState,
		   Mod, _Time, _Joins, _Debug) ->
    ok = try Mod:terminate()
	 catch
	     _:_ -> %% ignore
		 ok
	 end,
    Replies = 
	lists:map(
	  fun({_, {_, _}}=Tag) ->
		  %% END FIXME
		  {Tag, {reply, {error, Reason}}};
	     ({_, _})->
		  {none, noreply}
	  end, MsgTags),
    
    ok = handle_reply(Replies),
    stop_gen_joins(Reason, Name);    
handle_join_result({exit, Other}, _MsgTags, _Parent, _Name, _OldState,
		   _Mod, _Time, _Joins, _Debug) ->
    %% Invalid return from the handle_join call.
    %% Internal error. Closing server.
    exit({error, {internal, gen_joins_handle_join, Other}});
handle_join_result({Reply, NState}, MsgTags, Parent, Name, _OldState,
		   Mod, Time, Joins, Debug) ->
    ReplyTo =  lists:zip(MsgTags, Reply),
    ok = handle_reply(ReplyTo),
    decode_msg({internal, joins}, Parent, Name,
		NState, Mod, Time, Joins, Debug, false).

handle_reply([]) ->
    ok;
handle_reply([{_, noreply} | Rest]) ->
    %% FIXME: throw warning if tag not empty
    handle_reply(Rest);
handle_reply([{{_Tag, {To, Ref}}, {reply, Reply}} | Rest]) ->
    To ! {Ref, Reply},
    handle_reply(Rest).

%handle_msg({'$gen_call', From, Msg}, Parent, Name, State, Mod, _Time, Debug) ->
%    case catch Mod:handle_call(Msg, From, State) of
%	{reply, Reply, NState} ->
%	    Debug1 = reply(Name, From, Reply, NState, Debug),
%	    loop(Parent, Name, NState, Mod, infinity, Debug1);
%	{reply, Reply, NState, Time1} ->
%	    Debug1 = reply(Name, From, Reply, NState, Debug),
%	    loop(Parent, Name, NState, Mod, Time1, Debug1);
%	{noreply, NState} ->
%	    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, Name,
%				      {noreply, NState}),
%	    loop(Parent, Name, NState, Mod, infinity, Debug1);
%	{noreply, NState, Time1} ->
%	    Debug1 = sys:handle_debug(Debug, {?MODULE, print_event}, Name,
%				      {noreply, NState}),
%	    loop(Parent, Name, NState, Mod, Time1, Debug1);
%	{stop, Reason, Reply, NState} ->
%	    {'EXIT', R} = 
%		(catch terminate(Reason, Name, Msg, Mod, NState, Debug)),
%	    reply(Name, From, Reply, NState, Debug),
%	    exit(R);
%	Other ->
%	    handle_common_reply(Other, Parent, Name, Msg, Mod, State, Debug)
%    end;

stop_gen_joins(Reason, Name) ->
    %% FIXME: should we better handle the reason?
    unregister(Name),
    exit(Reason).

%%% ---------------------------------------------------
%%% Send/recive functions
%%% ---------------------------------------------------
do_send(Dest, Msg) ->
    case catch erlang:send(Dest, Msg, [noconnect]) of
	noconnect ->
	    spawn(erlang, send, [Dest,Msg]);
	Other ->
	    Other
    end.

%%% ---------------------------------------------------
%%% Terminate the server.
%%% ---------------------------------------------------

terminate(Reason, _Name, _Msg, Mod, State, _Joins, _Debug) ->
    case catch Mod:terminate(Reason, State) of
	{'EXIT', R} ->
%	    error_info(R, Name, Msg, State, Debug),
	    exit(R);
	_ ->
	    case Reason of
		normal ->
		    exit(normal);
		shutdown ->
		    exit(shutdown);
		_ ->
%		    error_info(Reason, Name, Msg, State, Debug),
		    exit(Reason)
	    end
    end.

%% Misc
%% FIXME: provide support for debugging
debug_options(_, _) ->
    [].

%%% -------------------------------------
%%% Joins handling functions
%%% -------------------------------------

gen_joins_mailbox() ->
    case get(?GEN_JOINS_MAILBOX) of
	undefined ->
	    {ok, jerlang_dict:init()};
	Mailbox ->
	    {ok, Mailbox}
    end.

gen_joins_duplicates() ->
    case get(?GEN_JOINS_DUPLICATES) of
	undefined ->
	    {ok, jerlang_2dict:init()};
	Mailbox ->
	    {ok, Mailbox}
    end.

store_mailbox(Mailbox) ->
    put(?GEN_JOINS_MAILBOX, Mailbox).

store_duplicates(Mapping) ->
    put(?GEN_JOINS_DUPLICATES, Mapping).

%% -----------------------------------------------
%% The 'none' case is used when we check joins
%% as a result of the previous join being successful,
%% i.e. found match. Since State could have changed
%% we need to check if we have any further matches.
%% -----------------------------------------------

%% FIXME: remove redundant code
process_join(none, Joins, State) ->
    {ok, Mailbox, D} = process_join_init(),
    process_join0(none, Joins, [], {Mailbox, {no, D}}, State);
process_join(Msg, Joins, State) ->
    {ok, Mailbox, D} = process_join_init(),
    {NMailbox, Idx} = jerlang_dict:insert(Mailbox, Msg),
    process_join0({Idx, Msg}, Joins, [], {NMailbox, {no, D}}, State).

process_join_init() ->
    {ok, Mailbox} = gen_joins_mailbox(),
    {ok, D} = gen_joins_duplicates(),
    {ok, Mailbox, D}.

process_join0(Msg, [], Joins, {M, {_, D}}, _) ->
    joins_matching({error, no_match}, Msg, 
		   lists:reverse(Joins),
		   {M, D});
process_join0(none, [Join | Rest], JResult, M, State) ->
    process_join1(none, [], Join, Rest, JResult, M, State);
process_join0({Id, Msg}, [Join | Rest], JResult, M, State) ->
    {NJoin, Flag} = alpha_reduction({Id, Msg}, Join),
    process_join1({Id, Msg}, Flag, NJoin, Rest, JResult, M, State).

process_join1(Msg, AReduce, Join, RestJoins, Result, MD, State) ->
    NMD = update_duplicates(Msg, AReduce, MD),
    {M, {_, D}} = NMD,
    Result1 = beta_reduction(Msg, AReduce, Join, {M, D}, State),
    case Result1 of
	{error, {not_found, NJoin}} ->
	    process_join0(Msg, RestJoins, [NJoin | Result], NMD, State);
	{ok, {MsgIds, NJoin}}  ->
	    joins_matching({ok, MsgIds}, Msg, 
			   {lists:reverse(Result), NJoin, RestJoins},
			   {M, D})
    end.

joins_matching({error, no_match}, _, Joins, {M, D}) ->
    store_mailbox(M),
    store_duplicates(D),
    {error, Joins};
joins_matching({ok, MsgIds}, InitMsg,
	       {TestedJs, Join, AfterJs}, {M, D}) ->

    {RemoveIds, PIds, All} =
	propagation_ids(MsgIds, Join),
    NAfterJs = propagation_update(InitMsg, PIds, AfterJs), 
    AllJoins = TestedJs ++ [Join] ++ NAfterJs,

    FinalJoins = purge_messages(AllJoins, RemoveIds),
    {NM, MatchedMsgs} = jerlang_dict:remove(M, All),
    ND = jerlang_2dict:remove(D, RemoveIds),
    store_mailbox(NM),
    store_duplicates(ND),
    {ok, {MatchedMsgs, FinalJoins}}.

propagation_update({Id, Msg}, PIds, Rest) ->
    case lists:member(Id, PIds) of
	true ->
	    lists:map(
	      fun(J) ->
		      {NJ, _} = alpha_reduction(
				  {Id, Msg}, J),
		      NJ
	      end, Rest);
	false ->
	    Rest
    end;
propagation_update(_, _, Rest) ->
    Rest.

alpha_reduction({Id, {_Tag, _From, Msg}}, [Alpha | Beta]) ->
    {NAlpha, Flag} = lists:mapfoldl(
      fun({PId, #pattern_joins{test=F, msgs=Msgs} = PRec} = P,
	  Acc) ->
	      case F(Msg) of
		  true ->
		      {{PId, PRec#pattern_joins{msgs=Msgs ++ [Id]}},
		       [PId | Acc]};
		  _ ->
		      {P, Acc}
	      end
      end, [], Alpha),
    {[NAlpha | Beta], Flag}.

%%  Function responsible for updating information
%%  about duplicate entries. Unfortunately one has to parse
%%  all the non-duplicates messages in order to find out
%%  groups that are equal to this message.
update_duplicates(none, _, M) ->
    M;
update_duplicates(_, [], M) ->
    M;
update_duplicates(_, _, {_, {yes, _}}=M) ->
    M;
update_duplicates({Elem, _}, _, {Mailbox, {no, Dupl}}) ->

    Update =
	jerlang_2dict:update(
	  Elem,
	  fun(Id) ->
		  jerlang_dict:get(Mailbox, Id)
	  end, Dupl),
    {Mailbox, {yes, Update}}.
    
% -spec(beta_reduction/5 ::
%       ({id(), msg()} | 'none', list(), join(),
%        {jdict(msg()), j2dict(ref(), [id()])}, state()) ->
% 	     {'error', {'not_found', join()}} |
% 		 {'ok', {[id()], join()}}).

-spec beta_reduction({id(), msg()} | 'none', list(), join(),
       {jdict(msg()), j2dict(ref(), [id()])}, state()) ->
	     {'error', {'not_found', join()}} |
		 {'ok', {[id()], join()}}.

beta_reduction({_, _}, [], Join, _MD, _State) ->
    {error, {not_found, Join}};
beta_reduction(Msg0, Matches, [Alpha | Beta], {M, D}, S) ->
    Id = case Msg0 of
	     {Id0, _} -> Id0;
	     none -> none
	 end,
    MAccess = fun(Ids) ->
		      lists:map(
			fun({_, _, Val}) ->
				Val;
			   ({_, Val}) ->
				Val;
			   (Val) ->
				Val
			end,jerlang_dict:get(M, Ids))
	      end,
    {Result, {Val1, NBeta}} = 
	beta_reduction1(Id, Matches, Alpha, Beta, [], {MAccess, D}, S),
    {Result, {Val1, [Alpha | NBeta]}}.

beta_reduction1(_, _, [], [], Result, _, _) ->
    {error, {not_found, lists:reverse(Result)}};
beta_reduction1(none, Matches,
		[{_, #pattern_joins{msgs=Msgs}}]=A, B, [], MD, S) ->
    %% Single pattern join - no new msg version.

    beta_reduction_for(Msgs, Matches, A, B, [], MD, S);    
beta_reduction1(Id, _Matches, [_A1], [{Fun, []}]=B, [], {M, _}, S) ->
    %% Single pattern join.
    Value = M([Id]),
    try Fun(Value, S) of
	true ->
	    {ok, {[Id], B}}
    catch
	error:function_clause ->
	    {error, {not_found, B}}
    end;
beta_reduction1(Id, Matches,
		[{_, #pattern_joins{msgs=MsgsA1}},
		 {_, #pattern_joins{msgs=MsgsA2}} | ARest],
		[{Fun, BMsgs} | BRest]=B, [],
		{M, D}, S) ->
    %% Initial Beta-reduction for first two messages.

    Type = case ARest of
		 [] -> final;
		 _  -> partial
	     end,
    FMsgsA1 = [[X] || X <- MsgsA1],
    Seq1 =
	try beta(FMsgsA1, MsgsA2,
		 {Type, BMsgs, {Fun, S, M}, D}) of
	    {ok, []} ->
		%%  Nothing new was found
		%%  {error, {not_found, B}};
		[];
	    {ok, Seq} ->
		Seq
	catch
	    error:_ ->
		exit({error, invalid_beta_reduction})
	end,
    Seq2 = BMsgs ++ Seq1,
    case Seq2 of
	[] ->
	    %%  This beta reduction isn't satisfyable
	    %%  by any set of messages. Abort. 
	    {error, {not_found, B}};
	_ ->
	    case Type of
		final ->
		    {ok, {Seq2, B}};
		_ ->
		    beta_reduction1(Id, Matches, ARest,
				    BRest, [{Fun, Seq2}],
				    {M, D}, S)
	    end
    end;
beta_reduction1(_, _, [[_]], [{_, []}], [], _, _) ->
    %%  Keep dialyzer happy
    exit({error, internal_error});
beta_reduction1(_, _Matches, [A | []],
		[{Fun, []}]=B, ResBeta,
		{M, D}, S) ->
    %%  Final beta reduction.
    {_NumA, #pattern_joins{msgs=MsgA}} = A,
    [{_, LastB} | _] = ResBeta,
    case beta(LastB, MsgA, {final, [], {Fun, S, M}, D}) of
	{ok, []} ->
	    {error, {not_found, lists:reverse(ResBeta) ++ B}};
	{ok, Seq} ->
	    {ok, {Seq, lists:reverse(ResBeta) ++ B}}
    end;
beta_reduction1(_, _, [_ | _], [{_, _} | _], [], _, _) ->
    %%  Keep dialyzer happy
    exit({error, internal_error});
beta_reduction1(Id, Matches, [A | ARest],
		[{Fun, BMsgs} | BRest] = B, ResBeta, {M, D}, S) ->
    %%  Partial beta reduction
    {_NumA, #pattern_joins{msgs=MsgA}} = A,
    [{_, LastB} | _] = ResBeta,
    ResSeq =
	try beta(LastB, MsgA,
		 {partial, BMsgs, {Fun, S, M}, D}) of
	    {ok, []} ->
		[];
	    {ok, Seq} ->
		Seq
	catch
	    error:Other ->
		exit({error,
		      {internal_error,
		       invalid_beta_reduction,
		       Other}})
	end,
    
    NResBeta =  BMsgs ++ ResSeq,
    case NResBeta of
	[] ->
	    {error,
	     {not_found, lists:reverse(ResBeta) ++ B}};
	_ ->
	    beta_reduction1(Id, Matches, ARest, BRest,
			    [{Fun, NResBeta} | ResBeta],
			    {M, D}, S)
    end.

%%  Only allowed to use on single pattern join
%%  for none.
beta_reduction_for([], _, _, B, _, _, _) ->
    {error, {not_found, B}};
beta_reduction_for([Id | Rest], Matches, A, B, Res, MD, S) ->
    NRes = beta_reduction1(Id, Matches, A, B, Res, MD, S),
    case NRes of
	{error, {not_found, NB}} ->
	    beta_reduction_for(Rest, Matches, A, NB, Res, MD, S);
	_ -> NRes
    end.

purge_messages(Joins, Ids) ->
    purge_messages0(Joins, Ids, []).

purge_messages0([], _, Result) ->
    lists:reverse(Result);
purge_messages0([Join | JSRest], Ids, Result) ->
    NJoin = purge_messages1(Join, Ids),
    purge_messages0(JSRest, Ids, [NJoin | Result]).

purge_messages1([Alpha | Beta], Ids) ->
    NJoins = lists:map(
	       fun({Id, #pattern_joins{msgs=Msgs}=P}) ->
		       NMsgs = Msgs -- Ids,
		       {Id, P#pattern_joins{msgs=NMsgs}}
	       end, Alpha),
    NBeta = lists:map(
	      fun({Fun, _}) ->
		      %% FIXME: we could possibly leave some information?
%		      NBMsgs = lists:filter(
%				 fun(List) ->
%					 contains_list(Ids, List)
%				 end, BMsgs),
%		      {Fun, NBMsgs}
		      {Fun, []}
	      end, Beta),
    [NJoins | NBeta].

propagation_ids(Ids, [Alpha | _]) ->
    {A0, A1, A2} = 
	lists:foldl(
	  fun({Id,
	       {_, #pattern_joins{prop=yes}}},
	      {Acc0, Acc1, Acc2}) ->
		  {Acc0, [Id | Acc1], [{prop, Id} | Acc2]};
	     ({Id, _},
	      {Acc0, Acc1, Acc2}) ->
		  {[Id | Acc0], Acc1, [Id | Acc2]}
	  end,
	  {[], [], []},
	  lists:zip(Ids, Alpha)),
    {lists:reverse(A0),
     lists:reverse(A1),
     lists:reverse(A2)}.

verify_join({ok, Joins}) ->
    try lists:foreach(
	  fun([Alpha | Beta]) ->
		  NumA = length(Alpha),
		  NumB = length(Beta),
		  true = (NumA > 0),
		  case NumA of 
		      1 -> (1 = NumB);
		      _ -> (NumB = (NumA - 1))
		  end
	  end, Joins)
    catch
	error:_ ->
	    throw({error, joins_verification_failed})
    end,
    {ok, Joins}.
