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
            [L|do_parse_file(FD)]
    end.    


dirs() -> [up, right, down, left].
delta(up) -> {-1, 0};
delta(right) -> {0, 1};
delta(down) -> {1, 0};
delta(left) -> {0, -1}.
plus({R1, C1}, {R2, C2}) -> {R1+R2, C1+C2}.

find_coords(M, {StartRow, StartCol}) ->
    Id = matrix:get(M, StartRow, StartCol),
    find_coords(M, {StartRow, StartCol}, Id, [{StartRow, StartCol}]).

find_coords(M, {Row, Col}, Id, Region) ->
%    io:format("Checking: ~p ~p ~n", [Row, Col]),
    SurroundingCoords = lists:map(fun(Dir) -> plus(delta(Dir), {Row, Col}) end, dirs()),
    CoordsChecked = lists:filter(fun({R, C}) -> Id == matrix:safe_get(M, R, C) end, SurroundingCoords),
    CoordsUnvisited = lists:filter(fun(C) -> not lists:member(C, Region) end, CoordsChecked),
%    io:format("Newighbours worth checking: ~p~n", [CoordsUnvisited]),
    lists:foldl(fun(Coord, Acc) ->
        find_coords(M, Coord, Id, Acc)
    end, Region ++ CoordsUnvisited, CoordsUnvisited).

%find_coords(M, {Row, Col}, Id, Visited) ->
%    io:format("Checking ~p ~p (~c)~n", [Row, Col, Id]),
%    case matrix:safe_get(M, Row, Col) of
%        oob -> Visited;
%        Id ->
%            case lists:member({Row, Col}, Visited) of
%                true ->
%                    io:format("Already visited.~n"),
%                    Visited;
%                false ->
%                    Fn = fun(Dir) ->
%                        find_coords(M, plus({Row, Col}, delta(Dir)), Id, [{Row, Col}|Visited])
%                    end,
%                    lists:usort(lists:flatten([{Row, Col}|lists:map(Fn, dirs())]))
%            end;
%        _ ->
%            []
%    end.

list_or([]) -> false;
list_or([true|_]) -> true;
list_or([false|T]) -> list_or(T).

deep_list_member(X, [L|T]) when is_list(L) ->
    deep_list_member(X, L) or deep_list_member(X, T);
deep_list_member(X, L) when is_list(L) ->
    lists:member(X, L).

%find_next_region_start(_M, oob, _Id, _Regions) ->
%    not_found;
%find_next_region_start(M, {StartRow, StartCol}, Id, Regions) ->
%    io:format("Find next region start: ~p ~p (not ~c)~n", [StartRow, StartCol, Id]),
%    case matrix:find_row_wise(M, StartRow, StartCol, fun(X) -> X /= Id end) of
%        not_found -> not_found;
%        {NextRow, NextCol}=NextStart ->
%            case deep_list_member(NextStart, Regions) of
%                false -> {NextRow, NextCol};
%                true ->
%                    find_next_region_start(M, matrix:next_row_wise(M, NextRow, NextCol), Id, Regions)
%            end
%    end.

% Just find a square that is not in Regions already
find_next_region_start(M, Regions) ->
    find_next_region_start(M, 0, 0, Regions).
find_next_region_start(M, Row, Col, Regions) ->
    %matrix:find_row_wise(M, 0, 0, fun(X) -> io:format("Check: ~p in ~p~n", [X, Regions]),not deep_list_member(X, Regions) end).
%    io:format("find_next_region_start: ~p ~p ~n",[Row, Col]),
    case deep_list_member({Row, Col}, Regions) of
        false -> {Row, Col};
        true ->
            case matrix:next_row_wise(M, Row, Col) of
                oob -> not_found;
                {NextRow, NextCol} -> find_next_region_start(M, NextRow, NextCol, Regions)
            end
    end.

find_regions(M) ->
    lists:usort(find_regions(M, {0, 0}, [])).
find_regions(_M, not_found, Regions) ->
    Regions;
find_regions(M, {StartRow, StartCol}, Regions) ->
    NewRegions = [find_coords(M, {StartRow, StartCol})|Regions],
    Start = find_next_region_start(M, StartRow, StartCol, NewRegions),
%    io:format("Found new start: ~p when regions: ~p are found~n", [Start, length(NewRegions)]),
    find_regions(M, Start, NewRegions).


region_fence_price(Region) ->
    Perimeter = fun({Row, Col}) ->
        CheckNeighbour = fun(Dir, Acc) ->
            case lists:member(plus(delta(Dir), {Row, Col}), Region) of
                true -> Acc;
                false -> Acc+1
            end
        end,
        lists:foldl(CheckNeighbour, 0, dirs())
    end,
    length(Region) * lists:sum(lists:map(Perimeter, Region)).

fence_price(Regions) ->
    lists:sum(lists:map(fun(R) -> region_fence_price(R) end, Regions)).

test() ->
    false = list_or([false, false, false]),
    true = list_or([true, false, false]),
    true = list_or([false, false, true]),

    true = deep_list_member(x, [[[y], x]]),
    true = deep_list_member(x, [x]),
    false = deep_list_member(x, [[[{a,b,c}], y]]),
    false = deep_list_member(x, [[[{x,x,x}], y]]),
    
    true = deep_list_member({0,0}, [[{0,0},{0,1},{0,2},{0,3}]]),


    [[{0,0},{0,1},{0,2},{0,3}],
    [{1,0},{1,1},{2,0},{2,1}],
    [{1,2},{2,2},{2,3},{3,3}],
    [{1,3}],
    [{3,0},{3,1},{3,2}]] = solve:find_regions(solve:parse_file("test1.txt")),

    140 = fence_price(solve:find_regions(solve:parse_file("test1.txt"))),

    772 = fence_price(solve:find_regions(solve:parse_file("test2.txt"))),

    1930 = fence_price(solve:find_regions(solve:parse_file("test3.txt"))),

    1550156 = fence_price(solve:find_regions(solve:parse_file("input.txt"))),



    ok.