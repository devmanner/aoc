-module(solve).
-compile(nowarn_export_all).
-compile(export_all).


prio(X) when X =< $Z ->
    X - $A + 27;
prio(X) ->
    X - $a + 1.

intersect(A, B) ->
    sets:to_list(sets:intersection(sets:from_list(A), sets:from_list(B))).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, 0).
parse_file(FD, Prio) ->
    case file:read_line(FD) of
        eof -> Prio;
        {ok, L} ->
            Line = string:chomp(L),
            {Fst, Snd} = lists:split(string:length(Line) div 2, Line),
            parse_file(FD, lists:foldl(fun(X, Acc) -> prio(X) + Acc end, Prio, intersect(Fst, Snd)))
    end.

parse_file2(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file2(FD, 0).
parse_file2(FD, Prio) ->
    case file:read_line(FD) of
        {ok, L1} ->
            {ok, L2} = file:read_line(FD),
            {ok, L3} = file:read_line(FD),
            {R1, R2, R3} = {string:chomp(L1), string:chomp(L2), string:chomp(L3)},
            parse_file2(FD, lists:foldl(fun(X, Acc) -> prio(X) + Acc end, Prio, intersect(R3, intersect(R1, R2))));
        eof ->
            Prio
    end.


do1() ->
    parse_file("input.txt").

do2() ->
    parse_file2("input.txt").

test() ->
    16 = prio($p),
    38 = prio($L),
    42 = prio($P),
    22 = prio($v),
    20 = prio($t),
    19 = prio($s),

    157 = parse_file("test_input.txt"),


    70 = parse_file2("test_input.txt"),

    ok.
