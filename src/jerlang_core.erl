-module(jerlang_core).

-export([test_raw/0]).
-export([loop/3, process_info_jerlang/1]).
-import(jerlang_misc, [convert_now/1]).

-include("jerlang.hrl").

-define(QUEUE_NAME, mailbox).
-define(CASE_TEST_GUARD, test_entry).
-define(CASE_RUN_ACTION, run_all).

% -spec(process_info_jerlang/1 :: (pid()) -> 'ok' | list()).
% -spec(loop/3 :: ([alpha()],
% 		 [receive_fun_test()],
% 		 nonempty_improper_list(receive_timeout(), any()))
%       -> {'ok', any()}).

-spec process_info_jerlang(pid()) -> 'ok' | list().
-spec loop([alpha()],
		 [receive_fun_test()],
		 nonempty_improper_list(receive_timeout(), any()))
      -> {'ok', any()}.


check_queue() ->
    case get(?QUEUE_NAME) of
	undefined ->
	    {ok, jerlang_dict:init()};
	Mailbox ->
	    {ok, Mailbox}
    end.

check_invalid_timeout([{timeout, Timeout, _}])
  when (Timeout == infinity) ->
    ok;
check_invalid_timeout([{timeout, Timeout, _}])
  when (not(is_number(Timeout))) orelse (Timeout < 0) ->
    exit({error, {invalid_timeout, Timeout}});
check_invalid_timeout(_) ->
    ok.

process_info_jerlang(Pid) when (Pid == self()) ->
    %% FIXME: join the information
    %% stored in the internal mailbox with
    %% what standard command offers
    ok;
process_info_jerlang(Pid) ->
    erlang:process_info(Pid).

loop(Joins, Actions, Options) ->
    {ok, Mailbox} = check_queue(),
    ok = check_invalid_timeout(Options),
    receive_loop0(
      process_old(Joins, Actions,
		  Mailbox, jerlang_dict:all(Mailbox)),
      Options).

receive_loop0(Result, []) ->
    receive_loop0(Result, [{timeout, infinity, none}]);


receive_loop0({ok, Result}, _) ->
    Result;
receive_loop0({error, Joins, Actions, Mailbox},
	      [{timeout, Timeout, Action}]) ->

    Opt = #opt{timeout=Timeout,
	       t_action=Action,
	       timer=disabled},
    receive_loop(Joins, Actions, Mailbox, Opt).

receive_loop(Joins, Actions, Mailbox,
	    Opt) ->
    {Timeout, NOpt} = process_timeout(Opt),
    receive_with_timeout(Joins, Actions, Mailbox, NOpt, Timeout).

receive_with_timeout(_, _, Mailbox,
		     #opt{t_action=TAction},
		     Timeout) when (Timeout < 0) ->

    put(?QUEUE_NAME, Mailbox),
    TAction();    
receive_with_timeout(Joins, Actions, Mailbox, Opt, Timeout)
  when (Timeout >= 0) or (Timeout == infinity) ->
    receive
	M ->
	    NOpt = update_timeout(Opt),
	    message(
	       process_join(M, Joins, Actions, Mailbox),
	       Actions, NOpt)
    after Timeout ->
	    timeout_action(Joins, Actions, Mailbox, Opt)
    end.

process_timeout(#opt{timer=disabled} = Opt) ->
    {0, Opt};
process_timeout(#opt{timeout=infinity} = Opt) ->
    {infinity, Opt};
process_timeout(#opt{timeout=0} = Opt) ->
    {0, Opt};
process_timeout(#opt{timeout=Timeout} = Opt) when (Timeout < 0) ->
    {Timeout, Opt};
process_timeout(#opt{timeout=Timeout} = Opt) ->
    {Timeout, Opt#opt{s_time=(convert_now(erlang:timestamp()))}}.

update_timeout(#opt{timer=disabled} = Opt) ->
    Opt;
update_timeout(#opt{timeout=infinity} = Opt) ->
    Opt#opt{timer=disabled};
update_timeout(#opt{s_time=Time, timeout=Limit} = Opt) ->
    Opt#opt{timeout=(Limit-(convert_now(erlang:timestamp())-Time)),
	    timer=disabled}.

timeout_action(_, _, _, #opt{timer=enabled, t_action=TAction}) ->
    TAction();
timeout_action(Joins, Actions, Mailbox, Opt) ->
    receive_loop(Joins, Actions,
		 Mailbox, Opt#opt{timer=enabled}).
    

process_old(Joins, Actions, Mailbox, []) ->
    {error, Joins, Actions, Mailbox};
process_old(Joins, Actions, Mailbox, [IdMsg | Rest]) ->
    case process_join(IdMsg, Joins, Actions, [], Mailbox) of
	{ok, Result} ->
	    {ok, Result};
	{error, NJoins, Mailbox} ->
	    process_old(NJoins, Actions, Mailbox, Rest)
    end.

message({ok, Result}, _, _) ->
    Result;
message({error, NJoins, NMailbox}, Actions, Options) ->
    receive_loop(NJoins, Actions, NMailbox, Options).

process_join(Msg, JoinsTests, Actions, Mailbox) ->
    {NMailbox, Idx} = jerlang_dict:insert(Mailbox, Msg),
    process_join({Idx, Msg}, JoinsTests, Actions, [], NMailbox).

process_join(_, [], _, JoinsTests, Mailbox) ->
    {error, lists:reverse(JoinsTests), Mailbox};
process_join({Id, Msg}, [JoinTests | JoinsRest],
	     [JoinAction | ActionsRest],
	     JoinsTests, Mailbox) ->
    {NJoin, Update} = 
	lists:mapfoldl(
		fun({PId, #pattern_joins{test=F, msgs=Msgs} = PRec} = P, S) ->
			case F(Msg) of
			    true ->
				{{PId, PRec#pattern_joins{msgs=[Id | Msgs]}},
				 yes};
			    _ ->
				{P, S}
			end
		end, no, JoinTests),

    Result1 = check_joins(Update, NJoin, JoinAction, Id, Mailbox),
    case run_joins(Result1, JoinAction, Mailbox, NJoin) of
	{error, _} ->
	    process_join({Id, Msg}, JoinsRest, ActionsRest,
			 [NJoin | JoinsTests], Mailbox);
	Other  -> Other
    end.
    
check_joins(no, _, _, _, _) ->
    {error, no_match};
check_joins(yes, JoinPatterns, Action, Id, Mailbox) ->
    PMsgs = lists:map(
      fun({_, #pattern_joins{msgs=Msgs}}) ->
	      Msgs
      end, JoinPatterns),
    %% Either returns error or found set
    jerlang_mset:common_sets(
      PMsgs, {fun test_guards/2, {Action, Mailbox}, Id}).

%%  Decision is not to use bultin func guards in the try...catch
%%  region, since there can be a function call withing the
%%  receive instruction, so this would caught wrong exception.
%%  Instead we build separate function that only handles the
%%  arguments (for example the same ones - {A, B} and {B, C})
%%  and guards are transformed into simple boolean expression.
test_guards(MsgIds, {Action, Mailbox}) ->
    MatchedMsgs = jerlang_dict:get(Mailbox, MsgIds),
    try Action(MatchedMsgs, ?CASE_TEST_GUARD) of
	true ->
	    {ok, MsgIds, none};
	false ->
	    error
    catch
	error:{badmatch, _} ->
	    error
    end.

run_joins({error, no_match}, _, _, _) ->
    {error, not_found};
run_joins({ok, MsgIds, _}, Action, Mailbox, JoinDef) ->
    %% Filter ids which really need to be removed
    PropMsgIds = propagation_filter(MsgIds, JoinDef, []),
    {NMailbox, MatchedMsgs} = jerlang_dict:remove(Mailbox, PropMsgIds),
    put(?QUEUE_NAME, NMailbox),
    {ok, Action(MatchedMsgs, ?CASE_RUN_ACTION)}.

propagation_filter([], [], Result) ->
    lists:reverse(Result);
propagation_filter([Id | IdRest],
		   [{_, #pattern_joins{prop=no}} | Rest],
		   Result) ->
    propagation_filter(IdRest, Rest, [Id | Result]);
propagation_filter([Id | IdRest],
		   [{_, #pattern_joins{prop=yes}} | Rest],
		   Result) ->
    propagation_filter(IdRest, Rest, [{prop, Id} | Result]).

test_raw() ->
%    self() ! single,
    self() ! {one, 12},
    self() ! {two, 2},
    self() ! three,

%    A = 12,
%    C = 15,
    self() ! {four, 17},
    self() ! {five, 6},
    %%    self() ! five,
    R2 = loop(
	   [[{1, #pattern_joins{test=
			  fun(Value) ->
				  case Value of
				      single -> true;
				      _ -> false
				  end
			  end, msgs=[]}}],
	    [{2, #pattern_joins{test=
			  fun(Value) ->
				  case Value of
				      {four, _SomeInt} -> true;
				      _ -> false
				  end
			  end, msgs=[]}},
	     {3, #pattern_joins{test=
			  fun(Value) ->
				  case Value of
				      {five, _Int} -> true;
				      _ -> false
				  end
			  end, msgs=[]}}]],
   
    %% TODO how assignment in receive changes things?
	   [fun(Res1, Case) ->
		    [single] = Res1,
		    Ret = true,
		    case Case of
			test_entry -> Ret;
			run_all ->
			    io:format("Three !~n", []),
			    {finished, single}
		    end
	    end,
	    fun(Res2, Case) ->
		    [{four, Z}, {five, I}] = Res2,
		    Ret = is_integer(Z) andalso (Z > 15) and (I > 3),
		    case Case of
			test_entry -> Ret;
			run_all ->
			    io:format("Four !~n", []),
			    {finished_second, {Z, I}}
		    end
	    end],
	   [{timeout, 10000,
	     fun() ->
		     io:format("Timeout occured~n", []),
		     timeout_ok
	     end}]),

    io:format("Finished the second receive with: ~p~n", [R2]),
    ok.

%test() ->
%    C = 15,
%    A = 12,
%    self() ! {one, 12},
%    self() ! {two, 2},
%    self() ! three,
%
%    receive
%	three ->
%	    {ok, C};
%	{one, A} and {two, B} ->
%	    {ok, {A, B}}
%    end.
