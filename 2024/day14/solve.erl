-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    do_parse_file(FD).

do_parse_file(FD) ->
    case file:read_line(FD) of
        eof -> [];
        {ok, S} ->
            % p=0,4 v=3,-3
            [[], PX, PY, VX, VY] = re:split(string:chomp(S), "[ pv,=]+", [trim, {return, list}]),
            [{{list_to_integer(PX),list_to_integer(PY)},{list_to_integer(VX),list_to_integer(VY)}}|do_parse_file(FD)]
    end.

iterate({{PX, PY}, {VX, VY}}, N, XSize, YSize) ->
    {((PX + (VX * N) rem XSize) + XSize) rem XSize, ((PY + (VY * N) rem YSize) + YSize) rem YSize}.

count_quadrants(L, XSize, YSize) ->
    count_quadrants(L, XSize, YSize, dict:from_list([{q1, 0},{q2, 0},{q3, 0},{q4, 0}])).

count_quadrants([], _, _, Dict) ->
    {dict:fetch(q1, Dict), dict:fetch(q2, Dict), dict:fetch(q3, Dict), dict:fetch(q4, Dict)};
count_quadrants([{X, Y}|T], XSize, YSize, Dict) ->
    case (X == (XSize div 2)) or (Y == (YSize div 2)) of
        true -> 
            count_quadrants(T, XSize, YSize, Dict);
        false -> 
            case {(X < XSize/2), (Y < YSize/2)} of
                {true, true} -> count_quadrants(T, XSize, YSize, dict:update(q1, fun(V) -> V+1 end, Dict));
                {false, true} -> count_quadrants(T, XSize, YSize, dict:update(q2, fun(V) -> V+1 end, Dict));
                {true, false} -> count_quadrants(T, XSize, YSize, dict:update(q3, fun(V) -> V+1 end, Dict));
                {false, false} -> count_quadrants(T, XSize, YSize, dict:update(q4, fun(V) -> V+1 end, Dict))
            end 
    end.

stage1(List, N, XSize, YSize) ->
    R = lists:map(fun(X) -> iterate(X, N, XSize, YSize) end, List),
    {Q1, Q2, Q3, Q4} = count_quadrants(R, XSize, YSize),
    Q1*Q2*Q3*Q4.

do_print(L, X, Y) ->
    Fn = fun({Xn, Yn}, Acc) ->
            case {Xn, Yn} of
                {X, Y} -> Acc+1;
                _ -> Acc
            end
        end,
    N = lists:foldl(Fn, 0, L),
    case N of
        0 -> io:format(" ");
        _ -> io:format("~p", [N])
    end.

print(L, XSize, YSize) ->
    ForeachRow = fun(R) ->
        ForeachCol = fun(C) ->
            do_print(L, C, R)
        end,
        lists:foreach(ForeachCol, lists:seq(0, XSize-1)),
        io:format("~n")
    end,
    lists:foreach(ForeachRow, lists:seq(0, YSize-1)).

stage2(List, MaxIters, XSize, YSize) ->
    stage2(List, 1, MaxIters, XSize, YSize).
stage2(_List, MaxIters, MaxIters, _XSize, _YSize) ->
    not_found;
stage2(List, N, MaxIters, XSize, YSize) ->
    R = lists:map(fun(X) -> iterate(X, N, XSize, YSize) end, List),
    case check_if_christmas_tree(R) of
        true ->
            io:format("# FOUND AFTER ITERATIONS: ~p ####################################################~n", [N]),
            print(R, XSize, YSize),
            N;
        false ->
            stage2(List, N+1, MaxIters, XSize, YSize)
    end.

% Let's assume that no robots will be on the same spot when forming a christmas tree.
check_if_christmas_tree([]) ->
    true;
check_if_christmas_tree([H|T]) ->
    case lists:member(H, T) of
        true -> false;
        false -> check_if_christmas_tree(T)
    end.

test() ->
    {10, 6} = solve:iterate({{2,4},{2,-3}}, 4, 11, 7),
    {1, 3} = solve:iterate({{2,4},{2,-3}}, 5, 11, 7),

    12 = stage1(parse_file("test1.txt"), 100, 11, 7),

    226236192 = stage1(parse_file("input.txt"), 100, 101, 103),

    8168 = stage2(parse_file("input.txt"), 100000, 101, 103),

    ok.