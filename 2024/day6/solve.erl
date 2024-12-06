-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    List = lists:map(fun binary_to_list/1, re:split(Bin, "\n")),
    Cols = length(hd(List)),
    Rows = length(List),
    matrix:from_list_of_rows(lists:map(fun(X) -> [X] end, lists:flatten(List)), Rows, Cols).

next_rc($^, R, C) -> {R-1, C};
next_rc($>, R, C) -> {R, C+1};
next_rc($V, R, C) -> {R+1, C};
next_rc($<, R, C) -> {R, C-1}.

next_dir($^) -> $>;
next_dir($>) -> $V;
next_dir($V) -> $<;
next_dir($<) -> $^.

step(Matrix, Direction, R, C) ->
    Rows = matrix:rows(Matrix),
    Cols = matrix:cols(Matrix),
    {NextRow, NextCol} = next_rc(Direction, R, C),
    case (NextRow < 0) or (NextRow >= Rows) or (NextCol < 0) or (NextCol >= Cols) of
        true -> {oob, Matrix};
        false ->
            case matrix:get(Matrix, NextRow, NextCol) of
                [$#] ->
                    {Matrix, next_dir(Direction), R, C};
                [$.] ->
                    {matrix:set(Matrix, NextRow, NextCol, [Direction]), Direction, NextRow, NextCol};
                Something ->
                    case lists:member(Direction, Something) of
                        true -> {loop, Matrix};
                        false -> {matrix:set(Matrix, NextRow, NextCol, [Direction|Something]), Direction, NextRow, NextCol}
                    end
            end 
    end.

step_until_oob_or_loop(Matrix) ->
    Direction = $^,
    {SRow, SCol} = matrix:find_row_wise(Matrix, 0, 0, [Direction]),
    step_until_oob_or_loop(Matrix, Direction, SRow, SCol).
step_until_oob_or_loop(Matrix, Direction, Row, Col) ->
    case step(Matrix, Direction, Row, Col) of
        {oob, Matrix} ->
            {oob, Matrix};
        {loop, Matrix} ->
            {loop, Matrix};
        {NewMatrix, NewDirection, NewRow, NewCol} ->
            step_until_oob_or_loop(NewMatrix, NewDirection, NewRow, NewCol)
    end. 

is_visited(L) ->
    lists:member($^, L) or lists:member($>, L) or lists:member($V, L) or lists:member($<, L).

count_visited(Matrix) ->
    F = fun({_, _}, L, Acc) ->
            case is_visited(L) of
                true -> Acc+1;
                false -> Acc
            end
        end,
    matrix:foldl_row_wise(Matrix, F, 0).

find_obsticle_places(Fname) ->
    M = parse_file(Fname),
    {SRow, SCol} = matrix:find_row_wise(M, 0, 0, [$^]),
    {oob, M2} = step_until_oob_or_loop(M),
    CollectVisited = fun({R, C}, L, Acc) ->
            case is_visited(L) of
                true -> [{R, C}|Acc];
                false -> Acc
            end
    end,
    Visited = matrix:foldl_row_wise(M2, CollectVisited, []),

    CollectLoopObsticlePos = fun({R, C}, Acc) ->
        %% We skip overwriting the starting position with an obsticle
        case (R == SRow) and (C == SCol) of
            true -> Acc;
            false ->
                MTemp = matrix:set(M, R, C, [$#]),
                case step_until_oob_or_loop(MTemp) of
                    {oob, _} -> Acc;
                    {loop, _} -> [{R, C}|Acc]
                end
        end
    end,
    lists:foldl(CollectLoopObsticlePos, [], Visited).

test1(Fname) ->
    M = parse_file(Fname),
    {oob, Matrix} = step_until_oob_or_loop(M),
    count_visited(Matrix).

test2(Fname) ->
    length(find_obsticle_places(Fname)).

test() ->
    41 = test1("test.txt"),
    5239 = test1("input.txt"),

    6 = test2("test.txt"),
    1753 = test2("input.txt").
