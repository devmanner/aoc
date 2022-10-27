-module(solve).
-compile({no_auto_import,[round/1]}).

-compile(export_all).

circle(N) ->
    lists:map(fun(X) -> {X, 1} end, lists:seq(1, N)).

clean_empty(L) ->
    lists:filter(fun({_, N}) -> N /= 0 end, L).

round(L) ->
    round(L, []).

round([{_Id, 0}], Acc) ->
    clean_empty(lists:reverse(Acc));
round([{Id, N}], Acc) ->
    {FirstId, FirstN} = lists:last(Acc),
    clean_empty(lists:reverse([{Id, N+FirstN}|lists:droplast(Acc)] ++ [{FirstId, 0}]));
round([{Id, 0}|T], Acc) ->
    round(T, [{Id, 0}|Acc]);
round([{Id, N}, {NextId, NextN}|T], Acc) ->
    round([{NextId, 0}|T], [{Id, N+NextN}|Acc]).

play(N) when is_integer(N) ->
    play(circle(N));
play([{Id, _}]) ->
    Id;
play(L) ->
    play(round(L)).

do1() ->
    play(3012210).

test() ->
    3 = play(5),

    1830117 = do1(),

    ok.
