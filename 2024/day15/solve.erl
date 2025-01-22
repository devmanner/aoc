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

look_ahead(Matrix, {_, _}=RC, {_, _}=Delta) ->
    {Nr, Nc} = plus(RC, Delta),
    case matrix:safe_get(Matrix, Nr, Nc) of
        $O -> look_ahead(Matrix, {Nr, Nc}, Delta);
        $# -> stop;
        $. -> {Nr, Nc}
    end.

dir2delta("<") -> {0, -1};
dir2delta("^") -> {-1, 0};
dir2delta(">") -> {0, 1};
dir2delta("v") -> {1, 0}.

plus({R1, C1}, {R2, C2}) ->
    {R1+R2, C1+C2}.

move(Matrix, Moves) ->
    {StartRow, StartCol} = matrix:find_row_wise(Matrix, 0, 0, $@),
    move(matrix:set(Matrix, StartRow, StartCol, $.), {StartRow, StartCol}, Moves).

move(Matrix, _, []) ->
    Matrix;
move(Matrix, {R, C}, [Dir|T]) ->
%    matrix:print(Matrix, fun(Chr) -> io:format("~c", [Chr]) end),
    Delta = dir2delta(Dir),
    {DeltaRow, DeltaCol} = plus({R, C}, Delta),
    case matrix:safe_get(Matrix, DeltaRow, DeltaCol) of
        $# -> move(Matrix, {R, C}, T);
        $. -> move(Matrix, {DeltaRow, DeltaCol}, T);
        $O ->
            case look_ahead(Matrix, {R, C}, Delta) of
                stop -> move(Matrix, {R, C}, T);
                {EmptyR, EmptyC} ->
                    NewMatrix = matrix:set(matrix:set(Matrix, DeltaRow, DeltaCol, $.), EmptyR, EmptyC, $O),
                    move(NewMatrix, {DeltaRow, DeltaCol}, T)
            end 
    end.

gps_sum(Matrix) ->
    Fn = fun({Row, Col}, $O, Acc) ->
                Acc + (Row * 100) + Col;
            (_, _, Acc) ->
                Acc
        end,
    matrix:foldl_row_wise(Matrix, Fn, 0).

test(Fname) ->
    {Matrix, Moves} = parse_file(Fname),
    AfterMoves = move(Matrix, Moves),
    matrix:print(AfterMoves, fun(C) -> io:format("~c", [C]) end),
    Sum = gps_sum(AfterMoves),
    io:format("GPS Sum: ~p~n", [Sum]),
    Sum.    

test() ->
    10092 = test("test1.txt"),
    2028 = test("test2.txt"),
    1318523 = test("input.txt"),
    


    ok.