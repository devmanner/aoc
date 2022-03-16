-module(solve).
-compile(export_all).

-define(INPUT, 1362).
-define(MASSIVE, 9999999999999999).

count_bits(0) ->
    0;
count_bits(N) ->
    1 + count_bits(N band (N - 1)).

is_wall(X, Y) ->
    (count_bits((X*X + 3*X + 2*X*Y + Y + Y*Y) + ?INPUT) band 1) == 1.

acc(X, Y) ->
    case (X < 0) or (Y < 0) or is_wall(X, Y) of
        true -> wall;
        false -> space
    end.

draw_cell(X, Y) ->
    case is_wall(X, Y) of
        true -> io:format("#");
        false -> io:format(".")
    end.
draw(Xmax, Ymax) ->
    lists:foreach(fun(Y) ->
        lists:foreach(fun(X) ->
            draw_cell(X, Y) end, lists:seq(0, Xmax)),
        io:format("~n") end, lists:seq(0, Ymax)).


cell_cost(X, Y, Map) ->
    case is_wall(X, Y) of
        true -> unreachable;
        false ->
            maps:get({X, Y}, ?MASSIVE, Map)
    end.

min(L) ->
    F = fun (unreachable, Min) ->
                Min;
            (X, Min) ->
                case X < Min of
                    true -> X;
                    false -> Min
                end
            end,
    lists:foldl(F, ?MASSIVE, L).

update_cost(0, 0, Map) ->
    maps:put({0, 0}, 0, Map);
update_cost(X, Y, Map) ->
    Min = min([ cell_cost(X+1, Y, Map),
                cell_cost(X-1, Y, Map),
                cell_cost(X, Y+1, Map),
                cell_cost(X, Y-1, Map) ]),
    case Min+1 < maps:get({X, Y}, ?MASSIVE, Map) of
        true -> maps:put({X, Y}, Min+1, Map);
        false -> Map
    end.

update(Xmax, Ymax, Map) ->
    F = fun(Y, YMap) -> 
            lists:foldl(fun(X, M) -> update_cost(X, Y, M) end, YMap, lists:seq(0, Xmax))
        end,
    case lists:foldl(F, Map, lists:seq(0, Ymax)) of
        Map -> io:format("Done!");
        NewMap -> update(Xmax, Ymax, NewMap)
    end.

test() ->
    1 = count_bits(1),
    1 = count_bits(2),
    2 = count_bits(3),
    1 = count_bits(1024),
    2 = count_bits(1025),
    
    wall = acc(-1,0),
    wall = acc(0,-1),

    ok.