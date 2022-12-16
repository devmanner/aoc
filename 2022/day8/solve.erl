-module(solve).
-compile(nowarn_export_all).
-compile(export_all).


parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    RawData = parse_file(FD, []),
    Rows = length(RawData),
    Cols = length(hd(RawData)),
    {Rows, Cols, array:from_list(lists:flatten(RawData))}.
parse_file(FD, Acc) ->
    case file:read_line(FD) of
        {ok, Data} ->
            Line = lists:map(fun(X) -> X - $0 end, string:chomp(Data)),
            parse_file(FD, [Line|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

is_visible(M, Row, Col) ->
    Rows = m_rows(M),
    Cols = m_cols(M),
    Height = m_get(M, Row, Col),

    LookFun =
        fun ({_R, _C}, false) -> false;
            ({R, C}, true) ->
                case solve:m_get(M, R, C) of
                    X when X < Height -> true;
                    _ -> false
                end
        end,
    lazy_or([
        %% Are we on the border?
        Row == 0,
        Col == 0,
        (Row+1) == Rows,
        (Col+1) == Cols,

        %% Look up
        fun() -> lists:foldl(LookFun, true, lists:map(fun(X) -> {X, Col} end, lists:seq(0, Row-1))) end,
        %% Look left
        fun() -> lists:foldl(LookFun, true, lists:map(fun(X) -> {Row, X} end, lists:seq(0, Col-1))) end,
        %% Look down
        fun() -> lists:foldl(LookFun, true, lists:map(fun(X) -> {X, Col} end, lists:seq(Row+1, Rows-1))) end,
        %% Look right
        fun() -> lists:foldl(LookFun, true, lists:map(fun(X) -> {Row, X} end, lists:seq(Col+1, Cols-1))) end
    ]).

lazy_or([]) ->
    false;
lazy_or([true|_]) ->
    true;
lazy_or([false|T]) ->
    lazy_or(T);
lazy_or([Fun|T]) when is_function(Fun) ->
    case Fun() of
        true -> true;
        false -> lazy_or(T)
    end.

n_visible(M) ->
    Rows = m_rows(M),
    Cols = m_cols(M),
    EachRow = fun(Row, Acc) ->
            EachCol = fun(Col, Acc2) ->
                    case is_visible(M, Row, Col) of
                        true -> Acc2+1;
                        false -> Acc2
                    end
                end,
            lists:foldl(EachCol, Acc, lists:seq(0, Cols-1))
        end,
    lists:foldl(EachRow, 0, lists:seq(0, Rows-1)).

clean({done, X}) -> X;
clean(X) -> X.

scenic_score(_M, Row, Col) when (Row == 0) or (Col == 0) ->
    0;
scenic_score(M, Row, Col) ->
    Rows = m_rows(M),
    Cols = m_cols(M),
    Height = m_get(M, Row, Col),

    Dist = fun
        ({_R, _C}, {done, Result}) ->
            {done, Result};
        ({R, C}, Acc) ->
            case m_get(M, R, C) of
                H when H < Height ->
                    Acc+1;
                _ ->
                    {done, Acc+1}
            end
    end,

    %% Look up
    U = clean(lists:foldl(Dist, 0, lists:map(fun(X) -> {X, Col} end, lists:reverse(lists:seq(0, Row-1))))),
    %% Look left
    L = clean(lists:foldl(Dist, 0, lists:map(fun(X) -> {Row, X} end, lists:reverse(lists:seq(0, Col-1))))),
    %% Look down
    D = clean(lists:foldl(Dist, 0, lists:map(fun(X) -> {X, Col} end, lists:seq(Row+1, Rows-1)))),
    %% Look right
    R = clean(lists:foldl(Dist, 0, lists:map(fun(X) -> {Row, X} end, lists:seq(Col+1, Cols-1)))),
    
%    io:format("U: ~p L: ~p D: ~p R: ~p~n", [U, L, D, R]),

    U*L*D*R.

max_scenic_score(M) ->
    Rows = m_rows(M),
    Cols = m_cols(M),
    EachRow = fun(Row, Acc) ->
            EachCol = fun(Col, Acc2) ->
                    max(scenic_score(M, Row, Col), Acc2)
                end,
            lists:foldl(EachCol, Acc, lists:seq(0, Cols-1))
        end,
    lists:foldl(EachRow, 0, lists:seq(0, Rows-1)).

% Representation of 2D matrix with 0-numbered row and col
m_new(Rows, Cols) ->
    {Rows, Cols, array:new(Rows*Cols)}.

m_get({_Rows, _Cols, Array}=Matrix, Row, Col) ->
    array:get(m_idx(Matrix, Row, Col), Array).

m_set({Rows, Cols, Array}=Matrix, Row, Col, Value) ->
    {Rows, Cols, array:set(m_idx(Matrix, Row, Col), Value, Array)}.

m_idx({Rows, Cols, _Array}, Row, Col) when (Col < Cols) and (Row < Rows) ->
    Row*Cols+Col.

m_rows({Rows, _Cols, _Array}) ->
    Rows.
m_cols({_Rows, Cols, _Array}) ->
    Cols.

do1() ->
    M = parse_file("input.txt"),
    n_visible(M).

do2() ->
    M = parse_file("input.txt"),
    max_scenic_score(M).

test() ->
    M = parse_file("test_input.txt"),
    true = is_visible(M, 1, 1),
    true = is_visible(M, 1, 2),
    false = is_visible(M, 1, 3),

    true = is_visible(M, 2, 1),
    false = is_visible(M, 2, 2),
    true = is_visible(M, 2, 3),

    false = is_visible(M, 3, 1),
    true = is_visible(M, 3, 2),
    false = is_visible(M, 3, 3),

    1546 = do1(),

    4 = scenic_score(M, 1, 2),
    8 = scenic_score(M, 3, 2),

    519064 = do2(),
    ok.

