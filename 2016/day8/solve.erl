-module(solve).
-compile(export_all).

matrix(R, C) ->
    {R, C, lists:map(fun(_) -> $. end, lists:seq(1, R*C))}.

acc({_Rows, Cols, M}, R, C) ->
    lists:nth(R*Cols+C+1, M).

% Rewrite if slow
acc({Rows, Cols, M}, R, C, V) ->
    {Before, A} = lists:split(R*Cols+C, M),
    [_|After] = A,
    {Rows, Cols, Before ++ [V] ++ After}.

print({Rows, Cols, _}=M) ->
    lists:foreach(fun(R) ->
        lists:foreach(fun(C) ->
            io:format("~c", [acc(M, R, C)])
        end, lists:seq(0, Cols-1)),
        io:format("~n")
    end, lists:seq(0, Rows-1)).


rect(M, R, C) ->
    lists:foldl(fun(Row, M1) ->
        lists:foldl(fun(Col, M2) ->
            acc(M2, Row, Col, $#)
        end, M1, lists:seq(0, C-1))
    end, M, lists:seq(0, R-1)).

rot_row({_, Cols, _}=M, Row, By) ->
    lists:foldl(fun(C, Acc) ->
        acc(Acc, Row, (C+By) rem Cols, acc(M, Row, C))
    end, M, lists:seq(0, Cols-1)).


rot_col({Rows, _, _}=M, Col, By) ->
    lists:foldl(fun(R, Acc) ->
        acc(Acc, (R+By) rem Rows, Col, acc(M, R, Col))
    end, M, lists:seq(0, Rows-1)).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, []).
parse_file(FD, Acc) ->
    case file:read_line(FD) of
        {ok, [$r,$e,$c,$t|T]} ->
            {ok, [Cols, Rows], _} = io_lib:fread("~dx~d", T),
            parse_file(FD, [{rect, Rows, Cols}|Acc]);
        {ok, [$r,$o,$t,$a,$t,$e,$ ,$c,$o,$l,$u,$m,$n,$ ,$x,$=|T]} ->
            {ok, [Col, By], _} =io_lib:fread("~d by ~d", T),
            parse_file(FD, [{rot_col, Col, By}|Acc]);
        {ok, [$r,$o,$t,$a,$t,$e,$ ,$r,$o,$w,$ ,$y,$=|T]} ->  
            {ok, [Row, By], _} =io_lib:fread("~d by ~d", T),
            parse_file(FD, [{rot_row, Row, By}|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

count_lit({_,_,L}) ->
    length(lists:filter(fun(X) -> X == $# end, L)).

do1_and_2() ->
    R = lists:foldl(fun({F, A1, A2}, Acc) ->
            apply(?MODULE, F, [Acc, A1, A2])
        end, matrix(6, 50), parse_file("input1.txt")),
    print(R),
    io:format("Lit: ~p~n", [count_lit(R)]).

test() ->
    {3, 3, "........-"} = acc(matrix(3,3), 2, 2, $-),
    {3, 3, "-........"} = acc(matrix(3,3), 0, 0, $-),
    {3, 3, ".-......."} = acc(matrix(3,3), 0, 1, $-),

    {3,3,"##.##...."} = rect(matrix(3, 3), 2, 2),
    {3,3,"######..."} = rect(matrix(3, 3), 2, 3),

    {3,3,"##..##..."} = rot_row(rect(matrix(3, 3), 2, 2), 1, 4),
    {3,3,"##.##...."} = rot_row(rect(matrix(3, 3), 2, 2), 1, 3),

    {4,4,"#...#....#...#.."} = rot_col(rect(matrix(4, 4), 2, 2), 1, 2),

    ok.
