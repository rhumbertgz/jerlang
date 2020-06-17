-module(jerlang_dict).

-export([init/0, insert/2, get/2, remove/2, all/1]).
-export([snapshot/1, size/1]).

-include("jerlang.hrl").

% -spec (init/0 :: () -> jdict(msg())).
% -spec(insert/2 :: (jdict(A), A) -> jdict(A)).
% -spec(get/2 :: (jdict(A), [integer()]) -> [A]).
% -spec(remove/2 :: (jdict(A), [integer() | {'prop', integer()}]) ->
% 	     {jdict(A), [A]}).
% -spec(all/1 :: (jdict(A)) -> [A]).
% -spec(size/1 :: (jdict(_)) -> integer()).
% -spec(snapshot/1 :: (jdict(A)) -> [A]).

-spec init() -> jdict(msg()).
-spec insert(jdict(A), A) -> jdict(A).
-spec get(jdict(A), [integer()]) -> [A].
-spec remove(jdict(A), [integer() | {'prop', integer()}]) -> {jdict(A), [A]}.
-spec all(jdict(A)) -> [A].
-spec size(jdict(_)) -> integer().
-spec snapshot(jdict(A)) -> [A].

init() ->
    {gb_trees:empty(), 1}.

insert({Tree, Id}, Element) ->
    {{gb_trees:enter(Id, Element, Tree), Id+1}, Id}.

get({Tree, _}, Id) when is_integer(Id) ->
    gb_trees:get(Id, Tree);
get(Tree, Ids) when is_list(Ids) ->
    get1(Tree, Ids, []).

get1(_, [], Elements) ->
    lists:reverse(Elements);
get1(Tree, [Id | Rest], Result) ->
    Value = jerlang_dict:get(Tree, Id),
    get1(Tree, Rest, [Value | Result]).

remove({Tree, Id}, RemoveList) ->
    {NTree, Result} = remove1(Tree, RemoveList, []),
    {{NTree, Id}, Result}.

remove1(Tree, [], Result) ->
    {Tree, lists:reverse(Result)};
remove1(Tree, [{prop, Id} | Rest], Result) ->
    Value = gb_trees:get(Id, Tree),
    remove1(Tree, Rest, [Value | Result]);
remove1(Tree, [Id | Rest], Result) ->
    Value = gb_trees:get(Id, Tree),
    NTree = gb_trees:delete(Id, Tree),
    remove1(NTree, Rest, [Value | Result]).

all({Tree, _}) ->
    gb_trees:to_list(Tree).

size({Tree, _}) ->
    gb_trees:size(Tree).

snapshot({Tree, _}) ->
    gb_trees:values(Tree).
