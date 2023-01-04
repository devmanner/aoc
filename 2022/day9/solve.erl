-module(solve).
-compile(nowarn_export_all).
-compile(export_all).


parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, []).
parse_file(FD, Acc) ->
    case io:fread(FD, "", "~c ~d") of
        {ok, [Dir, Steps]} ->
            parse_file(FD, [{dir2atom(Dir), Steps}|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

dir2atom([$R]) -> right;
dir2atom([$L]) -> left;
dir2atom([$U]) -> up;
dir2atom([$D]) -> down.

dir2delta(up) -> {-1, 0};
dir2delta(down) -> {1, 0};
dir2delta(left) -> {0, -1};
dir2delta(right) -> {0, 1}.

next({Row, Col}, {DRow, DCol}) ->
    {Row+DRow, Col+DCol}.

dist({Row1, Col1}, {Row2, Col2}) ->
    math:sqrt(math:pow(Row1-Row2, 2) + math:pow(Col1-Col2, 2)).

walk(StartPos, Instr) ->
    walk(StartPos, Instr, [StartPos]).

walk(_Pos, [], Acc) ->
    Acc;
walk({Row, Col}, [{_Dir, 0}|T], Acc) ->
    walk({Row, Col}, T, Acc);
walk({R, C}, [{Dir, Steps}|T], Acc) ->
    {TailRow, TailCol} = hd(Acc),
    {Row, Col} = next({R, C}, dir2delta(Dir)),
    case dist({Row, Col}, {TailRow, TailCol}) > 1.5 of
        true -> walk({Row, Col}, [{Dir, Steps-1}|T], [{R, C}|Acc]);
        false -> walk({Row, Col}, [{Dir, Steps-1}|T], Acc)
    end.

count_unique(L) ->
    S = sets:from_list(L),
    sets:size(S).

do1() ->
    count_unique(walk({0, 0}, parse_file("input.txt"))).

test() ->
    13 = count_unique(walk({0, 0}, parse_file("test_input.txt"))),
    ok.

