-module(jerlang_tests).

-ifdef(use_joins_vm).
-compile({parse_transform, jerlang_vm_parse}).
-else.
-compile({parse_transform, jerlang_parse}).
-endif.

-export([test_suite/0]).

-record(test0, {a, b, c, d}).
-record(test1, {one, two}).

test_suite() ->
    % ok = test_simple_receive(),
    ok = test_simple_joins(),
    % ok = test_timeout(),
    % ok = test_guards(),
    % ok = test_deep_receive(),
    % ok = test_propagation(),
    % ok = test_process_info(),
    % ok = test_receive_match(),
    % ok = test_compiler_warnings(),
    % ok = test_unbounded_variables(),
    % ok = test_remote_receive(),
    % ok = test_performance(),
    % ok = test_no_receive(),
    io:format("Finished test_suite...~n", []),
    ok.

clear_mailbox() ->
    receive
	M -> io:format("Mailbox entry: ~p~n", [M]),
	     clear_mailbox()
    after 0 ->
	    ok
    end.

test_simple_receive() ->
    io:format("Starting simple_receive test...~n", []),
    clear_mailbox(),
    self() ! {one, 4},
    ok = receive
	     {one, 4} ->
		 ok;
	     _ ->
		 error
	 end,

    self() ! invalid,
    self() ! {two, 1},
    ok = receive
	     {A, B} ->
		 A = two,
		 B = 1,
		 ok
	 end,
    ok.

test_simple_joins() ->
    io:format("Starting simple_joins test...~n", []),
    clear_mailbox(),
    self() ! {one, 1},
    self() ! {two, 2},
    self() ! {three, 3},
    
    
    ok = receive
	     {one, A} and {three, B} ->
			io:format("--->>> Join match...~n", []),
		 A = 1, B = 3, ok;
	     test ->
		 error
	 end,

    self() ! a,
    self() ! b,
    ok = receive
	     a and b->
		 error;
	     {two, 2} ->
		 ok
	 end,
    ok = receive
	     a and b ->
		 ok;
	     invalid2->
		 error
	 end,

    self() ! one,
    self() ! two,
    self() ! three,
    ok = receive
	     four ->
		 error;
	     one and two and three ->
		 ok
	 end,

    self() ! foo,
    self() ! foo, 
    self() ! foo,
    self() ! foo,
    ok = receive
	     foo and foo and foo and foo ->
		 ok
	 end,

    clear_mailbox(),
    self() ! {one, 1},
    self() ! {two, 3},
    Z = 4,
    A = 1,
    ok = receive
	     {two, 3} and {one, Z} ->
		 error;
	     {two, 3} and {one, A} ->
		 ok
	 end,
    self() ! {test, test1, test2},
    self() ! foo,
    ok = receive
	     _X and foo ->
		 error;
	     X ->
		 {test, _, _} = X,
		 ok
	 end,
    self() ! {test1, 10},
    self() ! {test2, 15},
    self() ! {test3, 10},
    ok = receive
	     {C, X1} and {B, X1} ->
		 try
		     C = test1,
		     B = test3,
		     ok
		 catch
		     error:{badmatch, _} ->
			 C = test3,
			 B = test1,
			 ok
		 end 
	 end,
    ok.

%%  This is partially tested by the clear_mailbox
%%  function
test_timeout() ->
    io:format("Starting timeout test...~n", []),
    clear_mailbox(),
    ok = receive
	     _ ->
		 error
	 after 0 ->
		 ok
	 end,
    
    self() ! {foo, 1},
    self() ! {foo, 2},

    ok = receive
	     {bar, _X, _Y} ->
		 error;
	     {foo, 3} ->
		 error
	 after 1000 ->
		 ok
	 end,
    B = -100,
    ok = try 
	     receive
		 _ ->
		     error
	     after B ->
		     error
	     end
	 catch
	     exit:{error, {invalid_timeout, _}} ->
		 ok
	 end,
		
    Self = self(),
    spawn(fun() ->
		  timer:sleep(1000),
		  Self ! {foo, bar, 100}
	  end),
    %% TODO: better ranges checking?
    ok = receive
	     {X, Y, Z} ->
		 X = foo, Y = bar, Z = 100, ok
	 after 3000 ->
		 error
	 end,

    self() ! unique1,
    self() ! unique2,
    self() ! unique3,
    ok = receive
	     unique1 and unique2 and unique3 ->
		 ok
	 after 0 ->
		 error
	 end,
    ok.

test_guards() ->
    io:format("Starting guards test...~n", []),
    clear_mailbox(),
    
    self() ! {foo, 12, 14},
    self() ! {bar, invalid},
    self() ! final,
    
    ok = receive
	     {foo, X1, _X2} and {bar, _}
	       when (X1 < 5) ->
		 error;
	     final ->
		 ok
	 end,

    ok = receive
	     {foo, X1, _X2} and {bar, _} when (X1 > 20) ->
		 error;
	     {foo, X1, _X2} and {bar, _} when (X1 < 20) ->
		 ok
	 end,

    self() ! {test1, 10},
    self() ! {test2, 20},
    self() ! {test3, 10},
    %%  Nondeterminism on which assignment is going to be choosen.
    %%  This actually depends on the implementation of the
    %%  oommon set algorithm
    ok = receive
	     {A, X1} and {_B, X2}
	       when (X1 == X2) andalso (A == test1) ->
		 ok;
	     {A, X1} and {_B, X2}
	       when (X1 == X2) andalso (A == test3) ->
		 ok
	 after 0 ->
		 error
	 end,

    ok.

test_deep_receive() ->
    io:format("Starting deep receive test...~n", []),
    clear_mailbox(),
   
    self() ! {foo, 1},
    self() ! {foo, 2},
    self() ! {foo, 3},
    self() ! bar,
    self() ! bar_test,
    
    ok = receive
	     {foo, _A} and {foo, _B} ->
		 receive
		     bar and {foo, 3} ->
			 ok
		 end;
	     {foo, 3} ->
		 error
	 end,

%    search_debug(on),
    self() ! {bar, 1},
    self() ! {bar, 2},
    self() ! {bar, 3},

    ok = receive
	     {bar, 1} and {bar, 2} and {bar, 3} ->
		 error;
	     bar_test and {bar, 2} ->
		 self() ! {bar, 4},
		 F = fun() ->
			     ok = receive
				 {bar, 4} ->
				     error;
				 {bar, 3} and {bar, 1} ->
				     ok
			     end,	     
			     ok
		     end,
		 F()
	 end,
    ok.

test_propagation() ->
    io:format("Starting propagation test...~n", []),
    clear_mailbox(),

    self() ! foo,
    self() ! bar,
    
    ok = receive
	     prop(foo) and prop(bar) ->
		 ok
	 end,
    ok = receive
	     foo and bar ->
		 ok
	 after 0 ->
		 error
	 end,
    ok = receive
	     foo ->
		 error;
	     bar ->
		 error
	 after 0 ->
		 ok
	 end,

    self() ! test1,
    self() ! test2,
    self() ! test3,
    ok = receive
	     test3 ->
		 error;
	     prop(test1) and test2 ->
		 ok
	 end,

    ok = receive
	     test3 ->
		 error;
	     prop(test1) ->
		 ok
	 end,

    self() ! second,
    ok = receive
	     second and test3 and test1 ->
		 ok
	 after 0 ->
		 error
	 end,

    ok.

test_process_info() ->
    io:format("Starting process_info test...~n", []),
    clear_mailbox(),
    ok.

test_receive_match() ->
    io:format("Starting receive_match test...~n", []),
    clear_mailbox(),

    %%  8 different cases
    %%  Although we check only few of them the whole
    %%  point is that they compile properly.
    ok = receive
	     _One and _Two and _Three ->
		 error
	 after 0 ->
		 ok
	 end,

    self() ! foo1,
    self() ! test1,
    self() ! test2,
    self() ! three,
    self() ! foo2,
    
    ok = receive
	     _One and _Two and Three=three ->
		 Three=three, 
		 ok
	 after 0 ->
		 error
	 end,
    clear_mailbox(),

    ok = receive
	     _One and Two=two and _Three ->
		 Two=two,
		 error
	 after 0 ->
		 ok
	 end,

    self() ! test1,
    self() ! three,
    self() ! three,
    self() ! two,
    ok = receive
	     One and Two=two and Three=three ->
		 Two=two,
		 Three=three,
		 true = (One == test1) or (One == three),
		 ok
	 after 0 ->
		 error
	 end,
    clear_mailbox(),
    ok = receive
	     _One=one and _Two and _Three ->
		 error
	 after 0 ->
		 ok
	 end,

    self() ! foo1,
    self() ! foo2,
    self() ! three,
    self() ! four,
    self() ! {one, b},
    ok = receive
	     One={one, Z} and Two and Three=three when is_atom(Z) ->
		 One={one, b},
		 Three=three,
		 true = (Two == foo1) or (Two == foo2) or (Two == four),
		 ok
	 after 0 ->
		 error
	 end,
    clear_mailbox(),
    ok = receive
	     One=one and Two=two and _Three ->
		 One=one,
		 Two=two,
		 error
	 after 0 ->
		 ok
	 end,
    self() ! foo1,
    self() ! three,
    self() ! foo2,
    self() ! one,
    self() ! foo3,
    self() ! two,
    ok = receive
	     One=one and Two=two and Three=three ->
		 One=one,
		 Two=two,
		 Three=three,
		 ok
	 after 0 ->
		 error
	 end,

    %%  More sophisticated matching
    
    clear_mailbox(),
    %%  Matching and propagation
    %%  Propagation is only allowed on the whole pattern
    %%  Including pattern. This avoids any problems that could
    %%  arise and we are consistent.
    Val1 = foo,
    self() ! {one, foo},
    self() ! one,
    self() ! foo,
    self() ! {two, foo},
    ok = receive
	     One=one and prop(Two=Val1) ->
		 One=one,
		 foo=Two,
		 ok
	 after 0 ->
		 error
	 end,
    ok.

test_compiler_warnings() ->
    io:format("Starting compiler_warnings test...~n", []),
    clear_mailbox(),
    Rec = #test0{a=1, b=2, c=3},
    #test0{b=B} = Rec,
    {_, _, A} = {foo, bar, test},
    
    ok = receive
	     A and B ->
		 A=test,
		 B=2,
		 error
	 after 0 ->
		 ok
	 end,
    ok = receive
	     %%  Expecting a single compiler warning on X
	     #test1{one=X} and #test1{two=B} ->	 
		 error
	 after 0 ->
		 ok
	 end,
    Bin = <<16#12345678:32/native>>,
    <<Head:10, _:10, Tail:12>> = Bin,
    Fixed = 10,
    ok = receive
	     {Head, Tail} and bar ->
		 error;
	     %%  Expecting compilation error on Test and Size
	     %%  but only once, as normally
%	     <<Head:Size/binary, 20:10>> and foo ->
	     <<Head:Fixed/binary, 20:10>> and foo ->
		 error
	 after 0 ->
		 ok
	 end,

    Val2 = bar,
    F = fun({ok, Arg1, Arg2}) ->
		%% pre: Arg1 == Arg2
		receive
		    {Arg1, Z} ->
			Arg1 = Arg2,
			receive
			    Z ->
				Z = Val2,
				ok
			end
		after 0 ->
			error
		end
	end,
    self() ! bar,
    self() ! foo,
    self() ! {foo, bar},
    ok = F({ok, foo, foo}),
    ok.

test_unbounded_variables() ->
    io:format("Starting unbounded_variables test...~n", []),
    clear_mailbox(),
    self() ! {ok, foo, bar},
    ok = receive
	     {ok, _, _} ->
		 ok
	 after 0 ->
		 error
	 end,

    self() ! {bar, 1},
    self() ! {foo, 2},
    ok = receive
	     {foo, _} and {bar, _} ->
		 ok
	 after 0->
		 error
	 end,
    %% FIXME: need to test patterns like {ok, _, _}
    %% where both '_' are different
    ok.
test_no_receive() ->
    io:format("Starting no_receive test...~n", []),
    clear_mailbox(),

    self() ! test1,
    self() ! test2,

    ok = receive
	     after 1000 ->
		     ok
	 end,
    %% FIXME: This should correctly handle the case
    %% like receive end.
    ok.

test_remote_receive() ->
    io:format("Starting remote_receive test...~n", []),
    clear_mailbox(),
    ok.

test_performance() ->
    %%  Check the behaviour of the mailbox on huge
    %%  number of messages and joins
    io:format("Starting performance test...~n", []),
    clear_mailbox(),
    ok.


%%%% How the example would normally be represented
%%%% after applying parse_transform
%    ok = receive
%        {single, B, _} when (B > 10) ->
%	    {finished, single};
%	prop({four, Z}) and {five, I} when is_integer(Z)
%	                             andalso (Z > 15)
%	                             and (I > 3)->
%	    io:format("Four !~n", []),
%	    {finished_second, {Z, I}}
%    after 10000 ->
%	    io:format("Timeout occureed~n", []),
%	    timeout
%    end,
 

%    R2 = jerlang_single_mailbox:loop(
%	   [[{1, #pattern{test=
%			  fun(Value) ->
%				  case Value of
%				      single -> true;
%				      _ -> false
%				  end
%			  end, msgs=[]}}],
%	    [{2, #pattern{test=
%			  fun(Value) ->
%				  case Value of
%				      {four, SomeInt} -> true;
%				      _ -> false
%				  end
%			  end, msgs=[]}},
%	     {3, #pattern{test=
%			  fun(Value) ->
%				  case Value of
%				      {five, Int} -> true;
%				      _ -> false
%				  end
%			  end, msgs=[]}}]],
%   
    %% TODO how assignment in receive changes things?
%	   [fun(Result0, Case0) ->
%                    [single] = Result0,
%                    Ret = true  %% none guards
%                    case Case0 of
%                        test_entry -> Ret;
%                        run_all ->
%                            io:format("Three !~n", []),
%		             {finished, single}
%                    end
%	     end
%	    fun(Result1, Case1) ->
%                    [{four, Z}, {five, I}] = Result1,
%                    Ret= is_integer(Z) andalso (Z > 15) and (I > 3),
%                    case Case1 of
%                        test_entry -> Ret;
%                        run_all ->
%                            io:format("Four !~n", []),
%		             {finished_second, {Z, I}}
%                    end
%	     end],
%	   [{timeout, 10000,
%	     fun() ->
%		     io:format("Timeout occured~n", []),
%		     timeout_ok
%	     end}]),
