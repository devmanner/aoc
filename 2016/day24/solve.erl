-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, 0, 0, orddict:new(), []).
parse_file(FD, X, Y, Map, Places) ->
    case file:read(FD, 1) of
        {ok, "#"} ->
            parse_file(FD, X+1, Y, orddict:store({X, Y}, wall, Map), Places);
        {ok, "."} ->
            parse_file(FD, X+1, Y, orddict:store({X, Y}, inf, Map), Places);
        {ok, "\n"} ->
            parse_file(FD, 0, Y+1, Map, Places);
        eof ->
            {Map, Places};
        {ok, _N} ->
            parse_file(FD, X+1, Y, orddict:store({X, Y}, inf, Map), [{X, Y}|Places])
    end.

move(up, {X, Y}) -> {X, Y-1};
move(down, {X, Y}) -> {X, Y+1};
move(left, {X, Y}) -> {X-1, Y};
move(right, {X, Y}) -> {X+1, Y}.

min(L) ->
    % There is a trick here... integer() < atom()
    % meaning we dont have to check for wall and
    % can start with inf as min value.
    F = fun(X, Min) ->
        case X < Min of
            true -> X;
            false -> Min
        end
    end,
    lists:foldl(F, inf, L).

shortest_path(Map, From, Places) ->
    Map2 = orddict:store(From, 0, Map),
    F = fun({X, Y}, _, {Mx,My}) ->
            {max(X, Mx), max(Y, My)}
        end,
    {MaxX, MaxY} = orddict:fold(F, {-1, -1}, Map2),
    Map3 = shortest_path(Map2, 0, 0, MaxX, MaxY),
    lists:map(fun(Pos) -> {Pos, orddict:fetch(Pos, Map3)} end, Places).

shortest_path(Map, X, Y, MaxX, MaxY) ->
    case do_shortest_path(Map, X, Y, MaxX, MaxY, 0) of
        {0, Map2} -> Map2;
        {_, Map2} -> shortest_path(Map2, X, Y, MaxX, MaxY)
    end.

do_shortest_path(Map, _X, Y, _MaxX, MaxY, Updates) when (Y > MaxY) ->
    io:format("Done, #updates ~p~n", [Updates]),
    {Updates, Map};
do_shortest_path(Map, X, Y, MaxX, MaxY, Updates) when (X > MaxX) ->
    do_shortest_path(Map, 0, Y+1, MaxX, MaxY, Updates);
do_shortest_path(Map, X, Y, MaxM, MaxY, Updates) ->
    Cost = orddict:fetch({X, Y}, Map),
    case min(lists:map(fun(Dir) -> safe_get(move(Dir, {X, Y}), Map) end, [up, down, left, right])) of
        Min when (Min+1) < Cost ->
%            io:format("New shortest: ~p~n", [Min]),
            Map2 = orddict:store({X, Y}, Min+1, Map),
            do_shortest_path(Map2, X+1, Y, MaxM, MaxY, Updates+1);
        _ ->
            do_shortest_path(Map, X+1, Y, MaxM, MaxY, Updates)
    end.

% Surround the maze with walls
safe_get(Key, Map) ->
    case orddict:is_key(Key, Map) of
        false -> wall;
        true -> orddict:fetch(Key,Map)
    end.



test() ->
%    {Map, Places} = parse_file("test_input.txt"),
    ok.
