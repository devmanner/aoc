-module(lists_util).
-export([test/0]).
-export([swap/3]).
-export([index_of/2]).

swap(L, Idx, Idx) ->
    L;
swap([X, Y], _, _) ->
    [Y, X];
swap(L, Idx1, Idx2) when (Idx1 > Idx2)->
    swap(L, Idx2, Idx1);
swap(L, Idx1, Idx2) ->
    Head = lists:sublist(L, Idx1-1),
    Mid = lists:nthtail(Idx1, lists:sublist(L, Idx2-1)),
    Tail = lists:nthtail(Idx2, L),
    Val1 = lists:nth(Idx1, L),
    Val2 = lists:nth(Idx2, L),
    lists:flatten([Head, Val2, Mid, Val1, Tail]).

index_of(L, Fun) ->
    index_of(L, Fun, 1).
index_of([], _, _) ->
    nomatch;
index_of([H|T], Fun, Idx) ->
    case Fun(H) of
        true -> {match, Idx};
        false -> index_of(T, Fun, Idx+1)
    end.


%    {P1, P2} = {min(Idx1, Idx2), max(Idx1, Idx2)},
%    {L1, [Elem1 | T1]} = lists:split(P1+1, L),
%    {L2, [Elem2 | L3]} = lists:split(P2-P1, T1),
%    lists:append([L1, [Elem2], L2, [Elem1], L3]).

test() ->
    [1, 2, 3, 4] = swap(lists:seq(1, 4), 1, 1),
    [4, 2, 3, 1] = swap(lists:seq(1, 4), 1, 4),
    [4, 2, 3, 1] = swap(lists:seq(1, 4), 4, 1),
    [1, 3, 2, 4] = swap(lists:seq(1, 4), 2, 3),

    [2, 1] = swap(lists:seq(1, 2), 1, 2),
    [3, 2, 1] = swap(lists:seq(1, 3), 1, 3),
    [2, 1, 3] = swap(lists:seq(1, 3), 1, 2),

    nomatch = index_of([], fun(_) -> false end),
    {match, 2} = index_of([1, 2, 3], fun(X) -> X == 2 end),
    {match, 1} = index_of([1, 2, 1], fun(X) -> X == 1 end),
    nomatch = index_of([1,2,3], fun(_) -> false end),

    ok.




