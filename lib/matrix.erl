-module(matrix).
-export([new/2, new/3, from_list_of_rows/1, from_list_of_rows/3, get/2, get/3, set/3, set/4, rows/1, cols/1, test/0]).

%% Get the index of the next cell, row wise
-export([next_row_wise/3]).

%% Foldl row wise over the matrix
-export([foldl_row_wise/3]).

% Find an element by searching one row at a time starting with row 0.
% Return {Row, Col} or not_found
-export([find_row_wise/4]).

%% Set a value to [Row, Col] and do just nothing if oob.
-export([safe_set/3, safe_set/4]).

%% Set a value to [Row, Col] and return oob or 4:rth arg if oob.
-export([safe_get/2, safe_get/3, safe_get/4]).

-export([print/1, print/2]).

% Representation of 2D matrix with 0-numbered row and col
new(Rows, Cols) ->
    {Rows, Cols, array:new(Rows*Cols)}.

new(Rows, Cols, DefaultValue) ->
    {Rows, Cols, array:new(Rows*Cols, [{default, DefaultValue}])}.

from_list_of_rows(ListOfRows) ->
    Rows = length(ListOfRows),
    Cols = length(hd(ListOfRows)),
    {Rows, Cols, array:from_list(lists:flatten(ListOfRows))}.

from_list_of_rows(List, Rows, Cols) ->
    {Rows, Cols, array:from_list(List)}.

get({_Rows, _Cols, _Array}=Matrix, {Row, Col}) ->
    get(Matrix, Row, Col).
get({_Rows, _Cols, Array}=Matrix, Row, Col) ->
    array:get(idx(Matrix, Row, Col), Array).

set({_Rows, _Cols, _Array}=Matrix, {Row, Col}, Value) ->
    set(Matrix, Row, Col, Value).
set({Rows, Cols, Array}=Matrix, Row, Col, Value) ->
    {Rows, Cols, array:set(idx(Matrix, Row, Col), Value, Array)}.

idx({Rows, Cols, _Array}, Row, Col) when (Col < Cols) and (Row < Rows) ->
    Row*Cols+Col.

rows({Rows, _Cols, _Array}) ->
    Rows.
cols({_Rows, Cols, _Array}) ->
    Cols.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foldl_row_wise({_Rows, Cols, Array}, Function, InitialAcc) ->
    Fun = fun(Index, Element, Acc) ->
        Row = Index div Cols,
        Col = Index rem Cols,
        Function({Row, Col}, Element, Acc)
    end,
    array:foldl(Fun, InitialAcc, Array).

find_row_wise(Matrix, StartRow, StartCol, Fun) when is_function(Fun) ->
    case Fun(get(Matrix, StartRow, StartCol)) of
        true -> {StartRow, StartCol};
        false ->
            case next_row_wise(Matrix, StartRow, StartCol) of
                oob -> not_found;
                {NextRow, NextCol} -> find_row_wise(Matrix, NextRow, NextCol, Fun)
            end
    end;
find_row_wise(Matrix, StartRow, StartCol, Value) ->
    Fun = fun(X) -> X == Value end,
    find_row_wise(Matrix, StartRow, StartCol, Fun).

next_row_wise({Rows, Cols, _Array}=Matrix, Row, Col) when (Row < Rows) and (Col < Cols) ->
    do_next_row_wise(Matrix, Row, Col).

do_next_row_wise({Rows, Cols, _Array}, Row, Col) when ((Col+1) >= Cols) and ((Row+1) >= Rows) ->
    oob;
do_next_row_wise({_Rows, Cols, _Array}, Row, Col) when ((Col+1) >= Cols) ->
    {Row+1, 0};
do_next_row_wise({_Rows, _Cols, _Array}, Row, Col) ->
    {Row, Col+1}.

print(M) ->
    print(M, fun(X) -> io:format("~p ", [X]) end).
print(M, PrintFun) ->
    ForeachRow = fun(R) ->
        ForeachCol = fun(C) ->
            PrintFun(matrix:get(M, R, C))
        end,
        lists:foreach(ForeachCol, lists:seq(0, matrix:cols(M)-1)),
        io:format("~n")
    end,
    lists:foreach(ForeachRow, lists:seq(0, matrix:rows(M)-1)).

safe_set(Matrix, {Row, Col}, Value) ->
    safe_set(Matrix, Row, Col, Value).
safe_set(Matrix, Row, Col, Value) ->
    Rows = matrix:rows(Matrix),
    Cols = matrix:cols(Matrix),
    case (Row < 0) or (Col < 0) or (Row >= Rows) or (Col >= Cols) of
        true -> Matrix;
        false -> set(Matrix, Row, Col, Value)
    end.

safe_get(Matrix, {Row, Col}) ->
    safe_get(Matrix, Row, Col).
safe_get(Matrix, {Row, Col}, ValueIfOOB) ->
    safe_get(Matrix, Row, Col, ValueIfOOB);
safe_get(Matrix, Row, Col) ->
    safe_get(Matrix, Row, Col, oob).
safe_get(Matrix, Row, Col, ValueIfOOB) ->
    Rows = matrix:rows(Matrix),
    Cols = matrix:cols(Matrix),
    case (Row < 0) or (Col < 0) or (Row >= Rows) or (Col >= Cols) of
        true -> ValueIfOOB;
        false -> get(Matrix, Row, Col)
    end.

test() ->
    M1 = new(10, 10, empty),
    M2 = set(M1, 0, 0, 10),
    empty = get(M2, 5, 5),
    empty = get(M2, {5, 5}),
    10 = get(M2, 0, 0),

    M3 = set(M2, 2, 2, 666),
    M3 = set(M2, {2, 2}, 666),
    M4 = set(M3, 3, 0, 666),

    Fn = fun({R, C}, Value, M) ->
        matrix:set(M, R, C, Value)
    end,
    foldl_row_wise(matrix:new(3, 5, blarf), Fn, matrix:new(3, 5)),
    

    {2, 2} = find_row_wise(M4, 0, 0, 666),
    {2, 2} = find_row_wise(M4, 2, 2, 666),
    {3, 0} = find_row_wise(M4, 2, 3, 666),
    
    M4 = safe_set(M4, 0, -1, 100),
    M4 = safe_set(M4, {0, -1}, 100),
    M4 = safe_set(M4, -1, 0, 100),
    M4 = safe_set(M4, rows(M4), 0, 100),



    ok.

