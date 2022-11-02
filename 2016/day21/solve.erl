-module(solve).
-compile(export_all).


index_of(X, L) ->
    index_of(X, L, 0).
index_of(X, [X|_], N) ->
    N;
index_of(X, [_|T], N) ->
    index_of(X, T, N+1).

cmd({swap_pos, X, Y}, L) ->
    {P1, P2} = {min(X, Y), max(X, Y)},
    {L1, [Elem1 | T1]} = lists:split(P1, L),
    {L2, [Elem2 | L3]} = lists:split(P2-P1-1, T1), 
    lists:append([L1, [Elem2], L2, [Elem1], L3]);
cmd({swap_letter, X, Y}, L) ->
    P1 = index_of(X, L),
    P2 = index_of(Y, L),
    cmd({swap_pos, P1, P2}, L);
cmd({rotate, left, S}, L) ->
    Steps = S rem length(L),
    {L1, L2} = lists:split(Steps, L),
    L2 ++ L1;
cmd({rotate, right, S}, L) ->
    Steps = S rem length(L),
    {L1, L2} = lists:split(length(L) - Steps, L),
    L2 ++ L1;
cmd({rotate_pos, X}, L) ->
    P1 = index_of(X, L),
    case P1 >= 4 of
        true -> cmd({rotate, right, 1+1+P1}, L);
        false -> cmd({rotate, right, 1+P1}, L)
    end;
cmd({reverse, X, Y}, L) ->
    {P1, P2} = {min(X, Y), max(X, Y)},
    {L1, Rest} = lists:split(P1, L),
    {L2, L3} = lists:split(P2-P1+1, Rest),
    L1 ++ lists:reverse(L2) ++ L3;
cmd({move, X, Y}, L) when X < Y ->
    {L1, [V|Rest]} = lists:split(X, L),
    {L2, L3} = lists:split(Y-X, Rest),
    L1 ++ L2 ++ [V] ++ L3;
cmd({move, X, Y}, L) ->
    {L1, [V|Rest]} = lists:split(X, L),
    {L2, L3} = lists:split(Y, L1),
    L2 ++ [V] ++ L3 ++ Rest.

dmc({swap_pos, X, Y}, L) ->
    cmd({swap_pos, X, Y}, L);
dmc({swap_letter, X, Y}, L) ->
    cmd({swap_letter, X, Y}, L);
dmc({rotate, left, S}, L) ->
    cmd({rotate, right, S}, L);
dmc({rotate, right, S}, L) ->
    cmd({rotate, left, S}, L);
dmc({rotate_pos, X}, L) ->
    % There is not always just one solution to this. There may be two...
    % Both are valid but we assume that the test input has no such strings.
    [Rot] = find_rotation(X, L),
    Rot;
dmc({reverse, X, Y}, L) ->
    cmd({reverse, X, Y}, L);
dmc({move, X, Y}, L) ->
    cmd({move, Y, X}, L).

find_rotation(X, L) ->
    find_rotation(X, L, length(L), []).
find_rotation(_X, _L, 0, Matches) ->
    Matches;
find_rotation(X, L, N, Matches) when N > 0 ->
    Attempt = cmd({rotate, left, N}, L),
    Res = cmd({rotate_pos, X}, Attempt),
    case Res of
        L ->
            find_rotation(X, L, N-1, [Attempt|Matches]);
        _ ->
            find_rotation(X, L, N-1, Matches)
    end.

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, []).
parse_file(FD, L) ->
    case file:read_line(FD) of
        eof -> lists:reverse(L);
        {ok, Line} -> parse_file(FD, [string_to_cmd(string:chomp(Line))|L])
    end.

string_to_cmd(S) ->
    case string:split(S, " ", all) of
        ["move", "position", From, "to", "position", To] ->
            {move, list_to_integer(From), list_to_integer(To)};
        ["swap", "position", From, "with", "position", To] ->
            {swap_pos, list_to_integer(From), list_to_integer(To)};
        ["reverse", "positions", From, "through", To] ->
            {reverse, list_to_integer(From), list_to_integer(To)};
        ["rotate", Direction, Steps, "steps"] ->
            {rotate, list_to_atom(Direction), list_to_integer(Steps)};
        ["rotate", Direction, "1", "step"] ->
            {rotate, list_to_atom(Direction), 1};
        ["rotate", "based", "on", "position", "of", "letter", [Letter]] ->
            {rotate_pos, Letter};
        ["swap", "letter", [X], "with", "letter", [Y]] ->
            {swap_letter, X, Y}
    end.

do1() ->
    Cmd = parse_file("input.txt"),
    lists:foldl(fun(C, Acc) -> cmd(C, Acc) end, "abcdefgh", Cmd).

do2() ->
    Cmd = parse_file("input.txt"),
    lists:foldl(fun(C, Acc) -> dmc(C, Acc) end, "fbgdceah", lists:reverse(Cmd)).

test_inv(X, Len) ->
    L = lists:seq(1, Len),
    io:format("============~nTesting: ~p on ~p ~n", [X, L]),
    R1 = cmd({rotate_pos, X}, L),
    case dmc({rotate_pos, X}, R1) of
        L -> ok;
        Faulty ->
            io:format("FAILED: ~p~n", [Faulty])
    end.


test_all_inv() ->
    Cmds = [
        {swap_pos, 2, 5},
        {swap_letter, $a, $c},
        {rotate, right, 3},
        {rotate, left, 2},
        % Not testing rotate_pos since it's not always a sigle solution
        %{rotate_pos, $c},
        {reverse, 3, 5},
        {move, 1, 3}
    ],

    Test = fun(X) ->
        L = lists:seq($a, X),
        DoTest = fun(Cmd) ->
            io:format("===============================~n"),
            io:format("Running: cmd(~p) on ~s ", [Cmd, L]),
            R1 = cmd(Cmd, L),
            io:format("=> ~s~n", [R1]),
            io:format("Running: dmz(~p) on ~s ", [Cmd, R1]),
            R2 = dmc(Cmd, R1),
            io:format("=> ~s~n", [R2]),
            R2 = L
        end,
        lists:foreach(DoTest, Cmds)
    end,

    lists:foreach(Test, lists:seq($f, $z)).


test() ->
    [2, 1] = cmd({swap_pos, 0, 1}, [1, 2]),
    [2, 1, 3] = cmd({swap_pos, 0, 1}, [1, 2, 3]),
    [1, 3, 2] = cmd({swap_pos, 1, 2}, [1, 2, 3]),
    
    "foobar" = cmd({swap_letter, $f, $b}, "boofar"),

    [3,4,1,2] = cmd({rotate, left, 2}, [1,2,3,4]),
    [3,4,1,2] = cmd({rotate, left, 6}, [1,2,3,4]),
    [1,2,3,4] = cmd({rotate, left, 0}, [1,2,3,4]),
    [1,2,3,4] = cmd({rotate, left, 4}, [1,2,3,4]),
    [2,3,4,1] = cmd({rotate, left, 1}, [1,2,3,4]),

    [3,4,1,2] = cmd({rotate, right, 2}, [1,2,3,4]),
    [3,4,1,2] = cmd({rotate, right, 6}, [1,2,3,4]),
    [1,2,3,4] = cmd({rotate, right, 0}, [1,2,3,4]),
    [1,2,3,4] = cmd({rotate, right, 4}, [1,2,3,4]),
    [4,1,2,3] = cmd({rotate, right, 1}, [1,2,3,4]),

    [4,1,2,3] = cmd({rotate_pos, 1}, [1,2,3,4]),
    [2,3,4,1] = cmd({rotate_pos, 3}, [1,2,3,4]),
    [5,1,2,3,4] = cmd({rotate_pos, 5}, [1,2,3,4,5]),

    [1,3,2,4] = cmd({reverse, 1, 2}, [1,2,3,4]),
    [2,1] = cmd({reverse, 0, 1}, [1,2]),

    [2,3,4,1] = cmd({move, 0, 3}, [1,2,3,4]),
    [4,1,2,3] = cmd({move, 3, 0}, [1,2,3,4]),
    [2,1,3,4] = cmd({move, 0, 1}, [1,2,3,4]),
    [2,3,1,4] = cmd({move, 0, 2}, [1,2,3,4]),


    "ebcda" = cmd({swap_pos, 4, 0}, "abcde"),
    "edcba" = cmd({swap_letter, $d, $b}, "ebcda"),
    "abcde" = cmd({reverse, 0, 4}, "edcba"),
    "bcdea" = cmd({rotate, left, 1}, "abcde"),
    "bdeac" = cmd({move, 1, 4}, "bcdea"),
    "abdec" = cmd({move, 3, 0}, "bdeac"),
    "ecabd" = cmd({rotate_pos, $b}, "abdec"),
    "decab" = cmd({rotate_pos, $d}, "ecabd"),

    test_inv(1, 10),
    test_inv(2, 10),
    test_inv(3, 10),
    test_inv(4, 10),
%    test_inv(5, 10),
    test_inv(6, 10),
    test_inv(7, 10),
    test_inv(8, 10),
    test_inv(9, 10),
 %   test_inv(10,10),

 %   test_inv(1, 9),
 %   test_inv(8, 9),
    test_inv(9, 9),

    test_all_inv(),

    "bfheacgd" = do1(),
    "gcehdbfa" = do2(),
    ok.
