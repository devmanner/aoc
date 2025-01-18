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

list_or([]) -> false;
list_or([true|_]) -> true;
list_or([false|T]) -> list_or(T).

deep_list_member(X, [L|T]) when is_list(L) ->
    deep_list_member(X, L) or deep_list_member(X, T);
deep_list_member(X, L) when is_list(L) ->
    lists:member(X, L).

% Order is important. Clockwise.
dirs() -> [right, down, left, up].
delta(up) -> {-1, 0};
delta(right) -> {0, 1};
delta(down) -> {1, 0};
delta(left) -> {0, -1}.
plus({R1, C1}, {R2, C2}) -> {R1+R2, C1+C2}.

check_order(Dir) ->
    check_order(Dir, dirs()).
check_order(D2, [D1,D2,D3,_D4]) ->
    [D1, D2, D3];
check_order(D, [H|T]) ->
    check_order(D, T ++ [H]).


find_coords(M, {StartRow, StartCol}) ->
    Id = matrix:get(M, StartRow, StartCol),
    find_coords(M, {StartRow, StartCol}, Id, [{StartRow, StartCol}]).

find_coords(M, {Row, Col}, Id, Region) ->
    SurroundingCoords = lists:map(fun(Dir) -> plus(delta(Dir), {Row, Col}) end, dirs()),
    CoordsChecked = lists:filter(fun({R, C}) -> Id == matrix:safe_get(M, R, C) end, SurroundingCoords),
    CoordsUnvisited = lists:filter(fun(C) -> not lists:member(C, Region) end, CoordsChecked),
    lists:foldl(fun(Coord, Acc) ->
        find_coords(M, Coord, Id, Acc)
    end, Region ++ CoordsUnvisited, CoordsUnvisited).

% Just find a square that is not in Regions already
find_next_region_start(M, Regions) ->
    find_next_region_start(M, 0, 0, Regions).
find_next_region_start(M, Row, Col, Regions) ->
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
    find_regions(M, Start, NewRegions).


region_fence_price1(Region) ->
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

fence_price1(Regions) ->
    lists:sum(lists:map(fun(R) -> region_fence_price1(R) end, Regions)).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Part2

fence_price2(Regions) ->
    lists:sum(lists:map(fun(R) -> region_fence_price2(R) end, Regions)).

region_fence_price2(Region) ->
    length(Region) * count_edges(Region).

% Find x in the X region. We know that the square to the right of the result is not in the region.
% XXXXOO
% XXxOOO
% XXOOOO
find_border([H|_]=Region) ->
    find_border(Region, H).
find_border(Region, Coord) ->
    % We know its 'right' but we take it from dirs as we later on use the order in dirs
    ToTheRight = plus(delta(hd(dirs())), Coord),
    case lists:member(ToTheRight, Region) of
        true -> find_border(Region, ToTheRight);
        false -> Coord
    end.

in_region(Region, Coord) ->
    lists:member(Coord, Region).

count_edges(Region) ->
%    Fn = fun(Coord, Visited) ->
%        {Cnt1, Visited1} = case
    Border = find_border(Region),
    {Cnt, _} = count_edges(Region, Border, down, 0, []),
    Cnt.

count_edges(Region, Coord, Dir, Cnt, Visited) ->
    case in_region(Visited, {Coord, Dir}) of
        true -> {Cnt, Visited};
        false -> do_count_edges(Region, Coord, Dir, Cnt, [{Coord, Dir}|Visited])
    end.

do_count_edges(Region, Coord, down, Cnt, Visited) ->
    OneDown = plus(Coord, delta(down)),
    OneDownRight = plus(Coord, {1, 1}),
    OneDownLeft = plus(Coord, {1, -1}),
    case {in_region(Region, OneDownLeft), in_region(Region, OneDown), in_region(Region, OneDownRight)} of
        {_, true, false} -> count_edges(Region, OneDown, down, Cnt, Visited);
        {_, true, true} -> count_edges(Region, OneDownRight, right, Cnt+1, Visited);
        {_, false, _} -> count_edges(Region, Coord, left, Cnt+1, Visited)
    end;
do_count_edges(Region, Coord, left, Cnt, Visited) ->
    OneLeft = plus(Coord, delta(left)),
    OneLeftUp = plus(Coord, {-1, -1}),
    OneLeftDown = plus(Coord, {1, -1}),
    case {in_region(Region, OneLeftUp), in_region(Region, OneLeft), in_region(Region, OneLeftDown)} of
        {_, true, false} -> count_edges(Region, OneLeft, left, Cnt, Visited);
        {_, true, true} -> count_edges(Region, OneLeftDown, down, Cnt+1, Visited);
        {_, false, _} -> count_edges(Region, Coord, up, Cnt+1, Visited)
    end;
do_count_edges(Region, Coord, up, Cnt, Visited) ->
    OneUp = plus(Coord, delta(up)),
    OneUpRight = plus(Coord, {-1, 1}),
    OneUpLeft = plus(Coord, {-1, -1}),
    case {in_region(Region, OneUpLeft), in_region(Region, OneUp), in_region(Region, OneUpRight)} of
        {false, true, _} -> count_edges(Region, OneUp, up, Cnt, Visited);
        {true, true, _} -> count_edges(Region, OneUpLeft, left, Cnt+1, Visited);
        {_, false, _} -> count_edges(Region, Coord, right, Cnt+1, Visited)
    end;
do_count_edges(Region, Coord, right, Cnt, Visited) ->
    OneRight = plus(Coord, delta(right)),
    OneRightUp = plus(Coord, {-1, 1}),
    OneRightDown = plus(Coord, {-1, -1}),
    case {in_region(Region, OneRightUp), in_region(Region, OneRight), in_region(Region, OneRightDown)} of
        {false, true, _} -> count_edges(Region, OneRight, right, Cnt, Visited);
        {true, true, _} -> count_edges(Region, OneRightUp, up, Cnt+1, Visited);
        {_, false, _} -> count_edges(Region, Coord, down, Cnt+1, Visited)
    end.

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

    140 = fence_price1(solve:find_regions(solve:parse_file("test1.txt"))),

    772 = fence_price1(solve:find_regions(solve:parse_file("test2.txt"))),

    1930 = fence_price1(solve:find_regions(solve:parse_file("test3.txt"))),

    1550156 = fence_price1(solve:find_regions(solve:parse_file("input.txt"))),

    %% Part 2

    %% ##
    %% ##
    4 = count_edges([{0,0},{0,1},{1,1},{1,0}]),

    %% #
    4 = count_edges([{0,0}]),

    %% ##
    %%  #
    6 = count_edges([{0,0},{0,1},{1,1}]),

    %% ###
    %% #
    6 = count_edges([{0,0},{0,1},{0,2},{1,0}]),

    %% ####
    %% #
    6 = count_edges([{0,0},{0,1},{0,2},{0,3},{1,0}]),

    %% ###
    %%  #
    8 = count_edges([{0,0},{0,1},{0,2},{1,1}]),

    80 = fence_price2(solve:find_regions(solve:parse_file("test1.txt"))),
%    436 = fence_price2(solve:find_regions(solve:parse_file("test2.txt"))),

    ok.