-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    do_parse_file(FD).

do_parse_file(FD) ->
    M = parse_matrix(FD),
    Matrix = matrix:from_list_of_rows(M),
%    matrix:print(Matrix, fun(C) -> io:format("~c", [C]) end),

    Moves = parse_moves(FD),
    {Matrix, Moves}.

parse_matrix(FD) ->
    case file:read_line(FD) of
        {ok, "\n"} ->
            [];
        {ok, S} ->
            L = re:split(string:chomp(S), "", [trim, {return, list}]),
           [L|parse_matrix(FD)] 
    end.

parse_moves(FD) ->
    case file:read(FD, 1) of
        {ok, "\n"} -> parse_moves(FD);
        {ok, C} -> [C|parse_moves(FD)];
        eof -> []
    end.

look_ahead1(Matrix, {_, _}=RC, {_, _}=Delta) ->
    {Nr, Nc} = plus(RC, Delta),
    case matrix:safe_get(Matrix, Nr, Nc) of
        $O -> look_ahead1(Matrix, {Nr, Nc}, Delta);
        $# -> stop;
        $. -> {Nr, Nc}
    end.

dir2delta("<") -> {0, -1};
dir2delta("^") -> {-1, 0};
dir2delta(">") -> {0, 1};
dir2delta("v") -> {1, 0}.

plus({R1, C1}, {R2, C2}) ->
    {R1+R2, C1+C2}.

move1(Matrix, Moves) ->
    {StartRow, StartCol} = matrix:find_row_wise(Matrix, 0, 0, $@),
    move1(matrix:set(Matrix, StartRow, StartCol, $.), {StartRow, StartCol}, Moves).

move1(Matrix, _, []) ->
    Matrix;
move1(Matrix, {R, C}, [Dir|T]) ->
%    matrix:print(Matrix, fun(Chr) -> io:format("~c", [Chr]) end),
    Delta = dir2delta(Dir),
    {DeltaRow, DeltaCol} = plus({R, C}, Delta),
    case matrix:safe_get(Matrix, DeltaRow, DeltaCol) of
        $# -> move1(Matrix, {R, C}, T);
        $. -> move1(Matrix, {DeltaRow, DeltaCol}, T);
        $O ->
            case look_ahead1(Matrix, {R, C}, Delta) of
                stop -> move1(Matrix, {R, C}, T);
                {EmptyR, EmptyC} ->
                    NewMatrix = matrix:set(matrix:set(Matrix, DeltaRow, DeltaCol, $.), EmptyR, EmptyC, $O),
                    move1(NewMatrix, {DeltaRow, DeltaCol}, T)
            end 
    end.

gps_sum(Matrix) ->
            % Stage 1
    Fn = fun({Row, Col}, $O, Acc) ->
                Acc + (Row * 100) + Col;
            % Stage 2
            ({Row, Col}, $[, Acc) ->
                Acc + (Row * 100) + Col;
            (_, _, Acc) ->
                Acc
        end,
    matrix:foldl_row_wise(Matrix, Fn, 0).

test1(Fname) ->
    {Matrix, Moves} = parse_file(Fname),
    AfterMoves = move1(Matrix, Moves),
    matrix:print(AfterMoves, fun(C) -> io:format("~c", [C]) end),
    Sum = gps_sum(AfterMoves),
    io:format("GPS Sum: ~p~n", [Sum]),
    Sum.    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Part 2 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

convert_map(Matrix) ->
    Fn = fun({R, C}, $#, M) ->
                matrix:set(matrix:set(M, R, C*2, $#), R, C*2+1, $#);
            ({R, C}, $., M) ->
                matrix:set(matrix:set(M, R, C*2, $.), R, C*2+1, $.);
            ({R, C}, $@, M) ->
                matrix:set(matrix:set(M, R, C*2, $@), R, C*2+1, $.);
            ({R, C}, $O, M) ->
                matrix:set(matrix:set(M, R, C*2, $[), R, C*2+1, $])
        end,
    matrix:foldl_row_wise(Matrix, Fn, matrix:new(matrix:rows(Matrix), matrix:cols(Matrix)*2)).

move_dir("<") -> left;
move_dir(">") -> right;
move_dir("^") -> vert;
move_dir("v") -> vert.

do_move_pointer(Matrix, From, To) ->
    matrix:set(matrix:set(Matrix, From, $.), To, $@).

% From and To points to the [ char of the box
do_move_box(M, From, To) ->
    M1 = matrix:set(M, From, $.),
    M2 = matrix:set(M1, plus(From, {0, 1}), $.),
    M3 = matrix:set(M2, To, $[),
    matrix:set(M3, plus(To, {0, 1}), $]).

move2(Matrix, []) ->
    Matrix;
move2(Matrix, [Dir|T]) ->
    {R, C} = matrix:find_row_wise(Matrix, 0, 0, $@),
%    io:format("On: ~p and want to try move to: ~s~n", [{R, C}, Dir]),
%    matrix:print(Matrix, fun(Chr) -> io:format("~c", [Chr]) end),
    Delta = dir2delta(Dir),
    {DeltaRow, DeltaCol} = plus({R, C}, Delta),
    case matrix:safe_get(Matrix, DeltaRow, DeltaCol) of
        $# -> move2(Matrix, T);
        $. -> move2(do_move_pointer(Matrix, {R, C}, {DeltaRow, DeltaCol}), T);
        Box ->
            MoveDir = move_dir(Dir),
            FrontSide = case {Box, MoveDir} of 
                {$[, left} -> 0;
                {$], left} -> -1;
                {$[, right} -> 1;
                {$], right} -> 0;
                {_, vert} -> 0 % Might be wrong... 
            end,
%            io:format("Will try to move_box in position: ~p ~p ~n", [{DeltaRow, DeltaCol+FrontSide}, Delta]),
            case move_box(MoveDir, Matrix, Delta, {DeltaRow, DeltaCol+FrontSide}) of
                no_move -> 
%                    io:format("No move...~n"),
                    move2(Matrix, T);
                NewMatrix ->
%                    io:format("It moved!~n"),
                    move2(do_move_pointer(NewMatrix, {R, C}, {DeltaRow, DeltaCol}), T)
            end
    end.  

move_box(left, Matrix, Delta, {Row, Col}) ->
%    io:format("Move box left: ~p in ~p~n", [{Row, Col}, Delta]),
    {DeltaRow, DeltaCol} = plus({Row, Col}, Delta),
    case matrix:safe_get(Matrix, DeltaRow, DeltaCol) of
        $# -> no_move;
        $. ->
            do_move_box(Matrix, {Row, Col}, {DeltaRow, DeltaCol});
        $] ->
            case move_box(left, Matrix, Delta, {DeltaRow, DeltaCol-1}) of
                no_move -> no_move;
                M1 ->
                    do_move_box(M1, {Row, Col}, {DeltaRow, DeltaCol})
            end
    end;
move_box(right, Matrix, Delta, {Row, Col}) ->
%    io:format("Move box right: ~p in ~p~n", [{Row, Col}, Delta]),
    {DeltaRow, DeltaCol} = plus({Row, Col}, Delta),
    case matrix:safe_get(Matrix, DeltaRow, DeltaCol) of
        $# -> no_move;
        $. ->
            do_move_box(Matrix, {Row, Col-1}, {DeltaRow, DeltaCol-1});
        $[ ->
            case move_box(right, Matrix, Delta, {DeltaRow, DeltaCol+1}) of
                no_move -> no_move;
                M1 ->
                    do_move_box(M1, {Row, Col-1}, {DeltaRow, DeltaCol-1})
            end
    end;
move_box(vert, Matrix, Delta, {Row, Col}) ->
%    io:format("Move box vert: ~p in ~p~n", [{Row, Col}, Delta]),
    {LeftSide, RightSide} = case matrix:safe_get(Matrix, Row, Col) of 
        $[ -> {0, 1};
        $] -> {-1, 0}
    end,
    AboveLeft = plus({Row, Col+LeftSide}, Delta),
    AboveRight = plus({Row, Col+RightSide}, Delta),
    case {matrix:safe_get(Matrix, AboveLeft), matrix:safe_get(Matrix, AboveRight)} of
        {$#, _} -> no_move;
        {_, $#} -> no_move;
        {$., $.} ->
            do_move_box(Matrix, {Row, Col+LeftSide}, plus({Row, Col+LeftSide}, Delta));
        {$[, $]} ->
            case move_box(vert, Matrix, Delta, AboveLeft) of
                no_move -> no_move;
                M1 ->
                    do_move_box(M1, {Row, Col+LeftSide}, plus({Row, Col+LeftSide}, Delta))
            end;
        {$], $[} ->
            case move_box(vert, Matrix, Delta, AboveLeft) of
                no_move -> no_move;
                M1 ->
                    case move_box(vert, M1, Delta, AboveRight) of
                        no_move -> no_move;
                        M2 -> 
                            do_move_box(M2, {Row, Col+LeftSide}, plus({Row, Col+LeftSide}, Delta))
                    end
            end;
        {$., $[} ->
            case move_box(vert, Matrix, Delta, AboveRight) of
                no_move -> no_move;
                M1 ->
                    do_move_box(M1, {Row, Col+LeftSide}, plus({Row, Col+LeftSide}, Delta))
            end;
        {$], $.} ->
            case move_box(vert, Matrix, Delta, AboveLeft) of
                no_move -> no_move;
                M1 ->
                    do_move_box(M1, {Row, Col+LeftSide}, plus({Row, Col+LeftSide}, Delta))
            end
    end.  

test2(Fname) ->
    {Matrix, Moves} = parse_file(Fname),
    AfterMoves = move2(convert_map(Matrix), Moves),
    matrix:print(AfterMoves, fun(C) -> io:format("~c", [C]) end),
    Sum = gps_sum(AfterMoves),
    io:format("GPS Sum: ~p~n", [Sum]),
    Sum.

test() ->
    10092 = test1("test1.txt"),
    2028 = test1("test2.txt"),
    1318523 = test1("input.txt"),
    
    9021 = test2("test1.txt"),
    1337648 = test2("input.txt"),
    ok.