-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD}  = file:open(Fname, [read]),
    matrix:from_list_of_rows(do_parse_file(FD)).

do_parse_file(FD) ->
    case file:read_line(FD) of
        eof -> [];
        {ok, S} ->
            L = re:split(string:chomp(S), "", [{return, list}, trim]),
            [lists:map(fun(X) -> list_to_integer(X) end, L)|do_parse_file(FD)]
    end.

score_trail_head(M, R, C, Val) ->
    Res = find_trails(M, R, C, Val),
    length(lists:uniq(Res)).

find_trails(_M, R, C, 9) ->
    io:format("Found end: ~p ~p~n", [R, C]),
    [{R, C}];
find_trails(M, R, C, Val) ->
    io:format("Counting trails from: ~p ~p with val: ~p~n", [R, C, Val]),
    Fn = fun(Dir, Acc) ->
        {DR, DC} = delta(Dir),
        case Val + 1 == matrix:safe_get(M, R+DR, C+DC) of
            true -> [find_trails(M, R+DR, C+DC, Val+1)|Acc];
            false -> Acc
        end 
    end,
    Res = lists:uniq(lists:flatten(lists:foldl(Fn, [], dirs()))),
    io:format("R: ~p~n", [Res]),
    Res.

dirs() -> [up, right, left, down].

delta(up) -> {-1, 0};
delta(right) -> {0, 1};
delta(down) -> {1, 0};
delta(left) -> {0, -1}.

score_trail_heads(M) ->
    lists:sum(score_trail_heads(M, matrix:find_row_wise(M, 0, 0, 0))).
score_trail_heads(_M, not_found) ->
    [];
score_trail_heads(M, {StartRow, StartCol}) ->
    Score = score_trail_head(M, StartRow, StartCol, 0),
    case matrix:next_row_wise(M, StartRow, StartCol) of
        oob -> [Score];
        {NextRow, NextCol} ->
            [Score|score_trail_heads(M, matrix:find_row_wise(M, NextRow, NextCol, 0))]
    end.

test() ->
    36 = solve:score_trail_heads(solve:parse_file("test2.txt")),
    582 = solve:score_trail_heads(solve:parse_file("input.txt")),
    ok.

