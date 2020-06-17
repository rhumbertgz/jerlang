-record(pclause, {joins, guards, cont}).
-record(pjoin, {channel, pattern, id}).
-record(pcontext, {module}).

-define(L(Val), io:format("~b ~p~n~n", [?LINE, Val])).
