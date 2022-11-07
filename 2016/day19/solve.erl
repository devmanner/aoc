-module(solve).
-compile({no_auto_import,[round/1]}).
-compile(nowarn_export_all).
-compile(export_all).

% List of {Id, NPresents}
circle(N) ->
    lists:map(fun(X) -> {X, 1} end, lists:seq(1, N)).

clean_empty(L) ->
    lists:filter(fun({_, N}) -> N /= 0 end, L).

round(L) ->
    round(L, []).

% We have reached the end of the list and the last one has no presents left
round([{_Id, 0}], Acc) ->
    clean_empty(lists:reverse(Acc));
% We have reached the end of the circle and the opponent is the first element in the list.
round([{Id, N}], Acc) ->
    {_FirstId, FirstN} = lists:last(Acc),
    clean_empty(lists:reverse([{Id, N+FirstN}|lists:droplast(Acc)]));
round([{Id, 0}|T], Acc) ->
    round(T, [{Id, 0}|Acc]);
round([{Id, N}, {NextId, NextN}|T], Acc) ->
    round([{NextId, 0}|T], [{Id, N+NextN}|Acc]).

play(N) when is_integer(N) ->
    play(circle(N));
play([{Id, _}]) ->
    Id;
play(L) ->
    io:format("Playing a round. ~p users left~n", [length(L)]),
    play(round(L)).

do1() ->
    play(3012210).

test() ->
    3 = play(5),

    1830117 = do1(),

    ok.
