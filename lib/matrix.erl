-module(matrix).
-export([new/2, new/3, from_list_of_rows/1, from_list_of_rows/3, get/3, set/4, rows/1, cols/1, test/0]).

%% Foldl row wise over the matrix
-export([foldl_row_wise/3]).

% Find an element by searching one row at a time starting with row 0.
% Return {Row, Col} or not_found
-export([find_row_wise/4]).

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

get({_Rows, _Cols, Array}=Matrix, Row, Col) ->
    array:get(idx(Matrix, Row, Col), Array).

set({Rows, Cols, Array}=Matrix, Row, Col, Value) ->
    {Rows, Cols, array:set(idx(Matrix, Row, Col), Value, Array)}.

idx({Rows, Cols, _Array}, Row, Col) when (Col < Cols) and (Row < Rows) ->
    Row*Cols+Col.

rows({Rows, _Cols, _Array}) ->
    Rows.
cols({_Rows, Cols, _Array}) ->
    Cols.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

foldl_row_wise({Rows, Cols, Array}, Function, InitialAcc) ->
    Fun = fun(Index, Element, Acc) ->
        Row = Index div Rows,
        Col = Index rem Cols,
        Function({Row, Col}, Element, Acc)
    end,
    array:foldl(Fun, InitialAcc, Array).

find_row_wise(Matrix, StartRow, StartCol, Fun) when is_function(Fun) ->
    case Fun(get(Matrix, StartRow, StartCol)) of
        true -> {StartRow, StartCol};
        false ->
            Rows = rows(Matrix),
            Cols = cols(Matrix),
            case {StartRow+1, StartCol+1} of
                {Rows, Cols} -> not_found;
                {_, Cols} -> find_row_wise(Matrix, StartRow+1, 0, Fun);
                _ -> find_row_wise(Matrix, StartRow, StartCol+1, Fun)
            end
    end;
find_row_wise(Matrix, StartRow, StartCol, Value) ->
    Fun = fun(X) -> X == Value end,
    find_row_wise(Matrix, StartRow, StartCol, Fun).

test() ->
    M1 = new(10, 10, empty),
    M2 = set(M1, 0, 0, 10),
    empty = get(M2, 5, 5),
    10 = get(M2, 0, 0),

    M3 = set(M2, 2, 2, 666),
    M4 = set(M3, 3, 0, 666),

    {2, 2} = find_row_wise(M4, 0, 0, 666),
    {2, 2} = find_row_wise(M4, 2, 2, 666),
    {3, 0} = find_row_wise(M4, 2, 3, 666),
    
    ok.

