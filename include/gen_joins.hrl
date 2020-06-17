%% requests represents the list of join records representing
%% joins that can contain many patterns
%% ref - unique reference issued by the original process
%% used as a way of identifying the reply
%% oldmsgs - messages which finally didn't create the joins
%% but which were actually matching the pattern
-include("joins.hrl").
-record(gen_state, {patternsEts, joins, ref, msgs=#vector{}}).
-record(gen_pattern, {num, pid, id, msgs=[]}).
