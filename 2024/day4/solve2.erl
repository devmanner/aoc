-module(solve2).
-compile(nowarn_export_all).
-compile(export_all).


parse_file(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    matrix:from_list_of_rows(lists:map(fun binary_to_list/1, re:split(Bin, "\n"))).

has_xmas(Matrix, Row, Col) ->
    Rows = matrix:rows(Matrix),
    Cols = matrix:cols(Matrix),
    case (Row == 0) or (Col == 0) or (Row+1 == Rows) or (Col+1 == Cols) of
        true -> false;
        false ->
            SW = lists:sort([matrix:get(Matrix, Row-1, Col-1), matrix:get(Matrix, Row+1, Col+1)]),
            NE = lists:sort([matrix:get(Matrix, Row+1, Col-1), matrix:get(Matrix, Row-1, Col+1)]),
            (SW == "MS") and (NE == "MS")
    end.

search_word(Matrix) ->
    lists:filter(fun({_, _, X}) -> X end, search_word_from(Matrix, 0, 0)).
search_word_from(Matrix, Row, Col) ->
    case matrix:find_row_wise(Matrix, Row, Col, $A) of
        {R, C} ->
            Res = {R, C, has_xmas(Matrix, R, C)},
            Rows = matrix:rows(Matrix),
            Cols = matrix:cols(Matrix),
            case {R+1 == Rows, C+1 == Cols} of
                {true, true} -> Res;
                {false, true} -> [Res|search_word_from(Matrix, R+1, 0)];
                {_, _} -> [Res|search_word_from(Matrix, R, C+1)]
            end;
        not_found -> []
    end.

test() ->
    9 = length(search_word(parse_file("test2.txt"))),
    length(search_word(parse_file("input.txt"))).
