-type(status() :: 'final' | 'partial').
-type(dupl() :: any()).
-type(state() :: any()).
-type(id() :: integer()).

-type(methods() :: 'init' | 'init_joins' | 'handle_joins' | 'terminate').
-type(behaviour() :: [{methods(), integer()}]).

-type(erl_term() :: any()).

-type(tree(A) :: A).
-type(jdict(A) :: {tree(A), integer()}).
-type(j2dict(A, B) :: {tree(A), tree(B)}).
-type(msg() :: any()).

-type(fun_beta_test() :: fun(([any()], state()) -> 'true')).
-type(fun_alpha_test() :: fun((msg()) -> 'true' | false)).

-record(pattern_joins, {test :: fun_alpha_test(),
			msgs=[] :: [msg()],
			prop=no :: 'undefined' | 'yes' | 'no'}).

-type(alpha() :: [{id(), #pattern_joins{}}]).
-type(beta()  :: [fun_beta_test()]).
-type(join() :: nonempty_improper_list(alpha(), beta())).

%%  -----------------------------
%%  receive-specific section
%%  -----------------------------

-record(opt, {s_time :: non_neg_integer(),
	      timeout :: timeout(),
	      t_action :: fun(() -> any()),
	      timer :: 'disabled' | 'enabled'}).

-type(receive_fun_test() :: fun(([any()], atom()) -> any())).
-type(receive_fun_param(A) :: fun(([any()], atom()) -> A)).
-type(receive_timeout() :: {'timeout', timeout(), fun(() -> any())}).

