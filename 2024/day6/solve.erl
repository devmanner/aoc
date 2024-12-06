-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    matrix:from_list_of_rows(lists:map(fun(X) -> binary_to_list(X) end, re:split(Bin, "\n"))).

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
        true -> {Matrix, oob};
        false ->
            case matrix:get(Matrix, NextRow, NextCol) of
                $# ->
                    {Matrix, next_dir(Direction), R, C};
                Direction ->
                    {Matrix, loop};
                _ ->
                    {matrix:set(Matrix, NextRow, NextCol, Direction), Direction, NextRow, NextCol}
            end 
    end.

step_until_oob(Matrix, Direction, Row, Col) ->
    case step(Matrix, Direction, Row, Col) of
        {Matrix, oob} ->
            Matrix;
        {NewMatrix, NewDirection, NewRow, NewCol} ->
            step_until_oob(NewMatrix, NewDirection, NewRow, NewCol)
    end. 

step_until_oob_or_loop(Matrix, Direction, Row, Col) ->
    case step(Matrix, Direction, Row, Col) of
        {_M, oob} ->
%            {SRow, SCol} = matrix:find_row_wise(M, 0, 0, fun(X) -> (X == $^) or (X == $>) or (X == $V) or (X == $<) end),
            ok;
        {NewMatrix, NewDirection, NewRow, NewCol} ->
            step_until_oob(NewMatrix, NewDirection, NewRow, NewCol)
    end. 


count_visited(Matrix) ->
    F = fun
            (_, $., Acc) ->
                Acc;
            (_, $#, Acc) ->
                Acc;
            (_, $^, Acc) ->
                Acc+1;
            (_, $>, Acc) ->
                Acc+1;
            (_, $V, Acc) ->
                Acc+1;
            (_, $<, Acc) ->
                Acc+1
        end,
    matrix:foldl_row_wise(Matrix, F, 0).

test(Fname) ->
    M = parse_file(Fname),
    {SRow, SCol} = matrix:find_row_wise(M, 0, 0, $^),
    Matrix = step_until_oob(M, $^, SRow, SCol),
    count_visited(Matrix).

test() ->
    41 = test("test.txt"),
    5239 = test("input.txt").
