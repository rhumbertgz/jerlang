-module(jerlang_2dict).

-export([add/3, get/3, init/0, update/3, remove/2]).
-include("jerlang.hrl").

%%  FIXME: ignore warning until OTP R13
% -spec(init/0 :: () -> j2dict(ref(), [id()])).
% -spec(add/3 :: (id(), 'none', j2dict(A,B)) -> j2dict(A,B);
%       (id(), ref(), j2dict(A,B)) -> j2dict(A,B)).
% -spec(update/3 :: (id(), fun ((id()) -> msg()) , j2dict(A,B))
%       -> j2dict(A,B)).
% %%  FIXME: are 'get' types ok?
% -spec(get/3 :: (id () | [id()], [id()], j2dict(_, B)) -> {'ok', [B]}).
% -spec(remove/2 :: (j2dict(A,B), [id()]) -> j2dict(A,B)).

-spec init() -> j2dict(ref(), [id()]).
-type ref() :: reference().
-spec add(id(), 'none', j2dict(A,B)) -> j2dict(A,B)
        ; (id(), ref(), j2dict(A,B)) -> j2dict(A,B).
-spec update(id(), fun ((id()) -> msg()) , j2dict(A,B))
      -> j2dict(A,B).
-spec get(id () | [id()], [id()], j2dict(_, B)) -> {'ok', [B]}.
-spec remove(j2dict(A,B), [id()]) -> j2dict(A,B).


%%  -----------------------------------------
%%  This data structure consists of two maps, where
%%  the first one maps every entry to a unique
%%  reference, that is a key in the second map
%%  and groups together equal entries (according
%%  to some equality operation on some data).
%%  -----------------------------------------

init() ->
    {dict:new(), dict:new()}.

add(Elem, none, {S1, S2}) ->
    Ref = make_ref(),
    NS2 = dict:store(Ref, [Elem], S2),
    NS1 = dict:store(Elem, Ref, S1),
    {NS1, NS2};
add(Elem, Ref, {S1, S2}) ->
    NS1 = dict:store(Elem, Ref, S1),
    NS2 = dict:update(Ref, fun(Old) ->
				   [Elem | Old]
			   end, [Elem], S2),
    {NS1, NS2}.

update(Elem, MFun, {_, S2}=S) ->
%    io:format("Updating value: ~p~n", [Elem]),
    Value = MFun(Elem),
%    io:format("Value: ~p~n", [Value]),
    update0(Elem, parse_msg(Value), MFun, dict:fetch_keys(S2), S).

update0(Elem, _, _, [], S) ->
    add(Elem, none, S);
update0(Elem, Value, MFun, [Key | Rest], {S1, S2}=S) ->
%    io:format("Second~n", []),
    Values = dict:fetch(Key, S2),
    case check_values(Values, Value, MFun) of
	none ->
	    update0(Elem, Value, MFun, Rest, S);
	{ok, Idx} ->
	    Ref = dict:fetch(Idx, S1),
	    add(Elem, Ref, S)
    end.

get([P], List0, S) ->
    %% Special case for the first match
    List1 = lists:flatten(List0),
    {ok, List2} = jerlang_2dict:get(P, List1, S),
    {ok, [[X] || X <- List2]};
get(Elem, List, _) when is_list(Elem) ->
    {ok, List};
get(Elem, List, {S1, S2}) ->
    Ref = dict:fetch(Elem, S1),
    Values = dict:fetch(Ref, S2),
    {ok, List -- Values}.

remove(S, []) ->
    S;
remove(S, [E | Rest]) ->
    NS = remove(S, E),
    remove(NS, Rest);
remove({S1, S2}, Elem) ->
    Ref = dict:fetch(Elem, S1),
    NS1 = dict:erase(Elem, S1),
    Value = dict:fetch(Ref, S2),
    NS2 = case Value of
	      [Elem] ->
		  dict:erase(Ref, S2);
	      _ ->
		  dict:store(Ref, Value -- [Elem], S2)
	  end,
    {NS1, NS2}.

check_values([], _, _) ->
    none;
check_values([Idx | Rest], Value0, MFun) ->
    V = MFun(Idx),
    case parse_msg(V) == Value0 of
	true ->
	    {ok, Idx};
	false ->
	    check_values(Rest, V, MFun)
    end.

parse_msg({_, _, Value}) ->
    Value;
parse_msg({_, Value}) ->
    Value;
parse_msg(Value) ->
    %%  For VM version
    Value.
