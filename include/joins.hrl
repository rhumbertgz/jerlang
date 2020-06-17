%% requests represents the list of join records representing
%% joins that can contain many patterns
%% ref - unique reference issued by the original process
%% used as a way of identifying the reply
%% oldmsgs - messages which finally didn't create the joins
%% but which were actually matching the pattern
-record(vector, {list=[], id=1, old=[]}).
-record(status, {qs, requests=none, parent, ref=none, msgs=#vector{}}).
-record(pattern, {id, pid, f, t, msgs=[]}).
-record(join, {patternsEts, joins}).
