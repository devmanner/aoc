-module(solve).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, []).
parse_file(FD, Acc) ->
    case io:fread(FD, "", "~c~d, ") of
        {ok, [Dir, Steps]} ->
            parse_file(FD, [{Dir, Steps}|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

dir() ->
    [
        {0, 1}, % Up
        {1, 0}, % Right
        {0, -1}, % Down
        {-1, 0} % Left
    ].

next_dir("R", [A,B,C,D]) ->
    [B,C,D,A];
next_dir("L", [A,B,C,D]) ->
    [D,A,B,C].

travel1(L) ->
    travel1(0, 0, dir(), L).
travel1(X, Y, _Dir, []) ->
    {X, Y};
travel1(X, Y, Dir, [{Turn, Dist}|T]) ->
    NextDir = next_dir(Turn, Dir),
    {DX, DY} = hd(NextDir),
    NewX = X + (DX*Dist),
    NewY = Y + (DY*Dist),
    travel1(NewX, NewY, NextDir, T).


add_pos(X, Y, {_DX, _DY}, X, Y, Acc) ->
    case lists:search(fun({Xn, Yn}) -> (Xn == X) and (Yn == Y) end, Acc) of
        false ->
            io:format("Acc: ~p~n", [Acc]),
            {ok, Acc};
        {value, _} ->
            {collision, {X, Y}}
    end;
add_pos(X, Y, {DX, DY}, XEnd, YEnd, Acc) ->
    case lists:search(fun({Xn, Yn}) -> (Xn == X) and (Yn == Y) end, Acc) of
        false ->
            add_pos(X + DX, Y + DY, {DX, DY}, XEnd, YEnd, [{X, Y}|Acc]);
        {value, _} ->
            {collision, {X, Y}}
    end.

travel2(L) ->
    travel2(0, 0, [], dir(), L).
travel2(X, Y, Visited, Dir, [{Turn, Dist}|T]) ->
    NextDir = next_dir(Turn, Dir),
    {DX, DY} = hd(NextDir),
    NewX = X + (DX*Dist),
    NewY = Y + (DY*Dist),
    case add_pos(X, Y, {DX, DY}, NewX, NewY, Visited) of
        {ok, NewVisited} ->
            io:format("NV: ~p~n", [NewVisited]),
            travel2(NewX, NewY, NewVisited, NextDir, T);
        {collision, {SomeX, SomeY}} ->
            {SomeX, SomeY}
    end.

dist({X, Y}) ->
    X + Y.

do1() ->
    dist(travel1(parse_file("input.txt"))).

do2() ->
    dist(travel2(parse_file("input.txt"))).


test() ->
    {5, 0} =  solve:travel1([{"R", 5}]),
    {2, 3} =  solve:travel1([{"R", 2}, {"L", 3}]),
    {0, -2} =  solve:travel1([{"R", 2}, {"R", 2}, {"R", 2}]),
    12 =  dist(solve:travel1([{"R", 5}, {"L", 5}, {"R", 5}, {"R", 3}])),

    236 = do1(),


    4 = dist(solve:travel2([{"R", 8}, {"R", 4}, {"R", 4}, {"R", 8}])),


    ok.



