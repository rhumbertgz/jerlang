-module(jerlang_gen_joins_test_main).

-export([test_suite/0, time_santa/0]).
-export([test_santa_claus_limited/1]).
-export([time_santa_script/1]).
-export([worker0/4]).
-import(jerlang_misc, [generate_seed/1, average/1]).

test_suite() ->
    ok = test_simple(),
    ok = test_propagation(),
    ok = test_simple_parsed(),
    ok = test_add_parsed(),
    ok = test_santa_claus(),
    ok = test_santa_claus_parse().

test_simple() ->
    test_simple(jerlang_gen_joins_test_calc).

test_simple_parsed() ->
    test_simple(jerlang_gen_joins_test_calc_parse).

test_simple(Module) ->
    Module:start(),
    Num = Module:result(),
    Module:elf(elf),
    Module:add(12),
    Module:elf({elf, 12}),
    timer:sleep(2000),
    Module:elf({elf, 39}),
    Module:add(10),
    true = ((Num + 22) == Module:result()),
    ok.


test_add_parsed() ->
    jerlang_gen_joins_test_calc_parse:start(),
    jerlang_gen_joins_test_calc_parse:result(),
    jerlang_gen_joins_test_calc_parse:add(12),
    jerlang_gen_joins_test_calc_parse:add(10),
    timer:sleep(1000),
    ok.

    

test_propagation() ->
    jerlang_gen_joins_test_prop:start(),
    ok = jerlang_gen_joins_test_prop:auth(natka),
    ok = jerlang_gen_joins_test_prop:auth(hubik),
    valid_pass = jerlang_gen_joins_test_prop:enter(natka),
    {auth, natka} = jerlang_gen_joins_test_prop:revoke(natka),
    Pid = spawn_link(
	    fun() ->
		    valid_pass =
			jerlang_gen_joins_test_prop:enter(natka),
		    timer:sleep(1000)
	  end),
    valid_pass = jerlang_gen_joins_test_prop:enter(hubik),
    timer:sleep(1000),
    ok = jerlang_gen_joins_test_prop:auth(natka),
    ok = try
	     jerlang_gen_joins_test_prop:enter(nobody),
	     error
	 catch
	     exit:{timeout, _} -> ok
	 end,
    ok = receive
	     {'EXIT', Pid, normal} -> ok
	 after 0 -> error
	 end.

test_santa_claus_parse() ->
    jerlang_gen_joins_test_santa_parse:start(),
    [spawn_link(?MODULE, worker0, [jerlang_gen_joins_test_santa_parse,
			      4, elf, Id]) || Id <- lists:seq(1,10)],
    [spawn_link(?MODULE, worker0, [jerlang_gen_joins_test_santa_parse,
			      4, reindeer, Id]) || Id <- lists:seq(1,9)],
    %% Could use joins here ? :)
    ok =
	receive
	    {'EXIT', _, A} when (A /= normal) ->
		io:format("EXIT: ~p~n", [A]),
		error
	after infinity ->
		ok
	end.


test_santa_claus_limited(Counter) ->
    jerlang_gen_joins_test_santa_parse:start(),
    [spawn_link(?MODULE, worker0,
		[jerlang_gen_joins_test_santa_parse,
		 Counter, elf, Id]) || Id <- lists:seq(1,10)],
    [spawn_link(?MODULE, worker0,
		[jerlang_gen_joins_test_santa_parse,
		 Counter, reindeer, Id]) || Id <- lists:seq(1,9)],
    ok = jerlang_gen_joins_test_santa_parse:status({Counter, Counter}),
    %% Could use joins here ? :)
    ok =
	receive
	    {'EXIT', _, A} when (A /= normal) ->
		io:format("EXIT: ~p~n", [A]),
		error
	after 0 ->
		ok
	end.

test_santa_claus() ->
    io:format("testing santa stuff~n", []),
    jerlang_gen_joins_test_santa:start(),
    [spawn_link(?MODULE, worker0,
		[jerlang_gen_joins_test_santa, 2, reindeer, Id])
     || Id <- lists:seq(1,9)],
    ok = receive
	     {'EXIT', _, A} when (A /= normal) ->
		 error
	 after 5000 -> ok
	 end.

worker0(Module, Num, Type, Id) ->
    generate_seed(Id),
    worker1(Module, Num, Type).

worker1(_Module, 0, _Type) ->
    ok;
worker1(Module, Num, Type) ->
    Res =
	try
	    ok = Module:Type(),
	    Done = list_to_atom(atom_to_list(Type) ++ "_done"),
	    Module:Done(),
	    ok
    catch
	_:_ ->
	    error
    end,
    case Res of
	ok ->
	    worker1(Module, Num-1, Type);
	error ->
	    ok
    end.

time_santa() ->
    Test = [2, 4, 8, 16, 32, 64, 128, 256, 512],
    Repeat = 5,

    lists:foreach(
      fun(C) ->
	      Res = lists:map(
		      fun(_) ->
			      {Time, _}
				  = timer:tc(?MODULE,
					     test_santa_claus_limited, [C]),
			      Time
		      end, lists:seq(1, Repeat)),
	      io:format("Santa ~p took: ~p seconds~n",
			[C, average(Res)/1000000])
      end, Test).

time_santa_script(Iter) ->
    {Time, _}
	= timer:tc(?MODULE,
		   test_santa_claus_limited, [Iter]),
    
    io:format("Santa ~p took: ~p seconds~n",
	      [Iter, Time/1000000]).
    
