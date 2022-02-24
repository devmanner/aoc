-module(solve).
-compile(export_all).

do1() ->
    do1("cxdnnyjw").
do1(S) ->
    do1(S, 0, []).
do1(_, _, L) when length(L) == 8 ->
    lists:reverse(lists:flatten(L));
do1(S, N, L) ->
    case crypto:hash(md5, S ++ integer_to_list(N)) of
        <<0, 0, 0:4, Key:4, _/binary>> ->
            io:format("~s~n", [S ++ integer_to_list(N)]),
            do1(S, N+1, [string:to_lower(integer_to_list(Key, 16))|L]);
        _ ->
            do1(S, N+1, L)
    end.

set_pos(L, Pos, _) when (Pos > 7) ->
    {0, L};
set_pos(L, Pos, Value) ->
    case lists:nth(Pos+1, L) of
        undef ->
            {1, lists:sublist(L, Pos) ++ [Value] ++ lists:nthtail(Pos+1, L)};
        _ ->
            {0, L}
    end.

do2() ->
    do2("cxdnnyjw").
do2(S) ->
    do2(S, 0, lists:map(fun(_) -> undef end, lists:seq(1, 8)), 0).
do2(_, _, L, 8) ->
    lists:flatten(L);
do2(S, N, L, Found) ->
    case crypto:hash(md5, S ++ integer_to_list(N)) of
        <<0, 0, 0:4, Pos:4, Key:4, _:4, _/binary>> ->
            io:format("~s~n", [S ++ integer_to_list(N)]),
            {Incr, L2} = set_pos(L, Pos, string:to_lower(integer_to_list(Key, 16))),
            do2(S, N+1, L2, Found+Incr);
        _ ->
            do2(S, N+1, L, Found)
    end.

test() ->
    "18f47a30" = do1("abc"),

    L = lists:map(fun(_) -> undef end, lists:seq(0, 7)),

    {1, [1, undef, undef, undef, undef, undef, undef, undef]} = set_pos(L, 0, 1),
    {1, [undef, undef, undef, undef, undef, undef, undef, 1]} = set_pos(L, 7, 1),
    {0, L} = set_pos(L, 8, 1),
    {1, L2} = set_pos(L, 0, something),
    {0, [something|_]} = set_pos(L2, 0, something_else),

    "05ace8e3" = do2("abc"),

    ok.

