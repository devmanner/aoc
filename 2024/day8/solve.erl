-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    matrix:from_list_of_rows(lists:map(fun binary_to_list/1, re:split(Bin, "\n"))).

find_antipols(Fname, AntipolsFun) ->
    M = parse_file(Fname),
%    Print = fun (undefined) -> io:format(".");
%                (X) -> io:format("~c", [X])
%            end,
%    matrix:print(M, Print),

    AP = find_antipols(M, [$.], matrix:new(matrix:rows(M), matrix:cols(M)), AntipolsFun),
%    io:format("OVERLAY~n"),
%    overlay_print(M, AP),
%    io:format("~n"),
    AP.


find_antipols(Matrix, Handled, Antipols, AntipolsFun) ->
    case matrix:find_row_wise(Matrix, 0, 0, fun(X) -> not (lists:member(X, Handled)) end) of
        not_found ->
            Antipols;
        {Row, Col} ->
            Freq = matrix:get(Matrix, Row, Col),
            CollectSenders = fun({R, C}, F, Acc) ->
                case (F == Freq) of
                    true -> Acc ++ [{R, C}];
                    false -> Acc
                end
            end,
            Senders = matrix:foldl_row_wise(Matrix, CollectSenders, []),
            F = fun({R, C}, Acc) ->
                add_antipols_for_sender(Matrix, R, C, Senders, Acc, AntipolsFun)
            end,
            find_antipols(Matrix, [Freq|Handled], lists:foldl(F, Antipols, Senders), AntipolsFun)
    end.

add_antipols_for_sender(_Matrix, _R, _C, [], Antipols, _AntipolsFun) ->
    Antipols;
add_antipols_for_sender(Matrix, R1, C1, [{R2, C2}|T], Antipols, AntipolsFun) ->
    APs = apply(?MODULE, AntipolsFun, [Matrix, R1, C1, R2, C2]),
    NewAntipols = lists:foldl(fun({R, C}, Acc) -> matrix:safe_set(Acc, R, C, $#) end, Antipols, APs),
    add_antipols_for_sender(Matrix, R1, C1, T, NewAntipols, AntipolsFun).

antipols1(_Matrix, Row, Col, Row, Col) ->
    [];
antipols1(_Matrix, Row1, Col1, Row2, Col2) ->
    RowDiff = Row2 - Row1,
    ColDiff = Col2 - Col1,
    [{Row1-RowDiff, Col1-ColDiff}, {Row2+RowDiff, Col2+ColDiff}].

antipols2(_Matrix, Row, Col, Row, Col) ->
    [];
antipols2(Matrix, Row1, Col1, Row2, Col2) ->    
    RowDelta = Row2 - Row1,
    ColDelta = Col2 - Col1,
    Rows = matrix:rows(Matrix),
    Cols = matrix:cols(Matrix),
    lists:usort(
        produce_antipols2(Row1, Col1, RowDelta, ColDelta, Rows-1, Cols-1) ++
        produce_antipols2(Row1, Col1, -1*RowDelta, -1*ColDelta, Rows-1, Cols-1)
    ).

produce_antipols2(Row, Col, _RowDelta, _ColDelta, MaxRow, MaxCol) when (Row < 0) or (Col < 0) or (Row > MaxRow) or (Col > MaxCol)  ->
    [];
produce_antipols2(Row, Col, RowDelta, ColDelta, MaxRow, MaxCol) ->
    [{Row, Col}|produce_antipols2(Row+RowDelta, Col+ColDelta, RowDelta, ColDelta, MaxRow, MaxCol)].

overlay_print(M1, M2) ->
    lists:foreach(
        fun(R) -> 
            lists:foreach(
                fun(C) ->
                    M1i = matrix:get(M1, R, C),
                    M2i = matrix:get(M2, R, C), 
                    case {M1i == $., M2i == undefined} of
                        {true, true} -> io:format(".");
                        {false, true} -> io:format("~c", [M1i]);
                        {true, false} -> io:format("~c", [M2i]);
                        {false, false} -> io:format(":")
                    end
                end, lists:seq(0, matrix:cols(M1)-1)),
                io:format("~n")
        end, lists:seq(0, matrix:rows(M1)-1)).                


test() ->
    [{0,0},{3,3}] = antipols1(dummy, 1,1,2,2),
    [{3,3},{0,0}] = antipols1(dummy, 2,2,1,1),
    [{0,0},{0,3}] = antipols1(dummy, 0,1,0,2),
    [{-2,1},{4,1}] = antipols1(dummy, 0,1,2,1),
    [{1,3},{7,6}] = antipols1(dummy, 3,4,5,5),
    [] = antipols1(dummy, 0,0,0,0),

    Count = fun (_, $#, Acc) -> Acc+1;
                (_, _, Acc) -> Acc
            end,
    R = find_antipols("test.txt", antipols1),
    14 = matrix:foldl_row_wise(R, Count, 0),

    R2 = find_antipols("input.txt", antipols1),
    240 = matrix:foldl_row_wise(R2, Count, 0),


    [{0,0},{1,3},{2,6},{3,9}] = antipols2({10, 10, dummy}, 0, 0, 1, 3),
    [{0,0},{1,3},{2,6},{3,9}] = antipols2({10, 10, dummy}, 1, 3, 0, 0),
    [{0,5},{1,3},{2,1}] = antipols2({10, 10, dummy}, 1, 3, 2, 1),
    [{0,0},{2,1},{4,2},{6,3},{8,4}] = antipols2({10, 10, dummy}, 2, 1, 0, 0),
    [] = antipols2(dummy, 0,0,0,0),

    R3 = find_antipols("test.txt", antipols2),
    34 = matrix:foldl_row_wise(R3, Count, 0),

    R4 = find_antipols("input.txt", antipols2),
    955 = matrix:foldl_row_wise(R4, Count, 0).









