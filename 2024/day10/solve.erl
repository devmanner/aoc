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
    io:format("Trails starting at: ~p ~p (~p) score: ~p are: ~n~p~n", [R, C, Val, length(Res), Res]),
    length(Res).

find_trails(M, R, C, Val) ->
    Filter = fun({Type, _}) -> Type == full end,
    T = lists:uniq(lists:filter(Filter, lists:flatten(find_trails(M, R, C, Val, [])))),
%    io:format("Trails: ~n~p~n", [T]),
    T.
find_trails(_M, R, C, 9, Trail) ->
%    io:format("Found end: ~p ~p~n", [R, C]),
    {full, [{R, C, 9}|Trail]};
find_trails(M, R, C, Val, Trail) ->
%    io:format("Counting trails from: ~p ~p with val: ~p~n", [R, C, Val]),
    Fn = fun(Dir) ->
        {DR, DC} = delta(Dir),
        case Val + 1 == matrix:safe_get(M, R+DR, C+DC) of
            true -> find_trails(M, R+DR, C+DC, Val+1, [{R, C, Val}|Trail]);
            false -> {dead_end, Trail}
        end 
    end,
    Res = lists:map(Fn, dirs()),
%    io:format("R: ~p~n", [Res]),
    Res.

dirs() -> [up, right, left, down].

delta(up) -> {-1, 0};
delta(right) -> {0, 1};
delta(down) -> {1, 0};
delta(left) -> {0, -1}.

score_trail_heads(M) ->
    score_trail_heads(M, matrix:find_row_wise(M, 0, 0, 0)).
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
%   Destroyed stage 1 in this version.... 
%    36 = solve:score_trail_heads(solve:parse_file("test2.txt")),
%    582 = solve:score_trail_heads(solve:parse_file("input.txt")),

    81 = lists:sum(score_trail_heads(parse_file("test2.txt"))),
    1302 = lists:sum(score_trail_heads(parse_file("input.txt"))).
    

