-module(solve).
-compile(nowarn_export_all).
-compile(export_all).


parse_file(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    matrix:from_list_of_rows(lists:map(fun binary_to_list/1, re:split(Bin, "\n"))).

next_rc(n, R, C) -> {R-1, C};
next_rc(ne, R, C) -> {R-1, C+1};
next_rc(e, R, C) -> {R, C+1};
next_rc(se, R, C) -> {R+1, C+1};
next_rc(s, R, C) -> {R+1, C};
next_rc(sw, R, C) -> {R+1, C-1};
next_rc(w, R, C) -> {R, C-1};
next_rc(nw, R, C) -> {R-1, C-1}.

search_word_in_direction(_Matrix, _Row, _Col, _Dir, []) ->
    found;
search_word_in_direction(Matrix, Row, Col, Direction, [H|Tail]) ->
    Rows = matrix:rows(Matrix),
    Cols = matrix:cols(Matrix),
    case (Row == Rows) or (Col == Cols) or (Col < 0) or (Row < 0) of
        true -> not_found;
        false ->
            case matrix:get(Matrix, Row, Col) of
                H ->
                    {NextRow, NextCol} = next_rc(Direction, Row, Col),
                    search_word_in_direction(Matrix, NextRow, NextCol, Direction, Tail);
                _ ->
                    not_found
            end 
    end.

search_word_all_directions(Matrix, Row, Col, Word) ->
    Directions = [n, ne, e, se, s, sw, w, nw],
    SearchWord = fun(Direction) ->
        {Row, Col, Direction, search_word_in_direction(Matrix, Row, Col, Direction, Word)}
    end,
    Filter = fun({_, _, _, Found}) ->
        Found == found
    end,
    lists:filter(Filter, lists:map(SearchWord, Directions)).

search_word(Matrix, Word) ->
    lists:flatten(search_word_from(Matrix, 0, 0, Word)).

search_word_from(Matrix, Row, Col, [H|_]=Word) ->
    io:format("search_word_from(MATRIX, ~p, ~p, ~s)~n", [Row, Col, Word]),
    case matrix:find_row_wise(Matrix, Row, Col, H) of
        {R, C} ->
            Res = search_word_all_directions(Matrix, R, C, Word),
            Rows = matrix:rows(Matrix),
            Cols = matrix:cols(Matrix),
            case {R+1 == Rows, C+1 == Cols} of
                {true, true} -> Res;
                {false, true} -> [Res|search_word_from(Matrix, R+1, 0, Word)];
                {_, _} -> [Res|search_word_from(Matrix, R, C+1, Word)]
            end;
        not_found -> []
    end.

test() ->
    18 = length(search_word(parse_file("test.txt"), "XMAS")),
    length(search_word(parse_file("input.txt"), "XMAS")).
