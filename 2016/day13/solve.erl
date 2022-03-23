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

draw_cell(X, Y, Map) ->
    case is_wall(X, Y) of
        true -> io:format("#");
        false ->
            case maps:get({X, Y}, Map, ?MASSIVE) =< 50 of
                true -> io:format("+");
                false -> io:format(" ")
            end
    end.
draw(Xmax, Ymax, Map) ->
    lists:foreach(fun(Y) ->
        lists:foreach(fun(X) ->
            draw_cell(X, Y, Map) end, lists:seq(0, Xmax)),
        io:format("~n") end, lists:seq(0, Ymax)).


cell_cost(X, Y, Map) ->
    case is_wall(X, Y) of
        true -> unreachable;
        false ->
            maps:get({X, Y}, Map, ?MASSIVE)
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

update_cost(1, 1, Map) ->
    maps:put({1, 1}, 0, Map);
update_cost(X, Y, Map) ->
    case acc(X, Y) of
        wall -> Map;
        space -> 
            Min = min([ cell_cost(X+1, Y, Map),
                        cell_cost(X-1, Y, Map),
                        cell_cost(X, Y+1, Map),
                        cell_cost(X, Y-1, Map) ]),
            case Min+1 < maps:get({X, Y}, Map, ?MASSIVE) of
                true -> maps:put({X, Y}, Min+1, Map);
                false -> Map
            end
    end.

update(Xmax, Ymax, Map) ->
    F = fun(Y, YMap) -> 
            lists:foldl(fun(X, M) -> update_cost(X, Y, M) end, YMap, lists:seq(0, Xmax))
        end,
    case lists:foldl(F, Map, lists:seq(0, Ymax)) of
        Map -> Map;
        NewMap -> update(Xmax, Ymax, NewMap)
    end.

do1() ->
    M = solve:update(50, 50, maps:new()),
    maps:get({31,39}, M).

do2() ->
    M = solve:update(50, 50, maps:new()),
    V = maps:values(M),
    length(lists:filter(fun(X) -> X =< 50 end, V)).

test() ->
    1 = count_bits(1),
    1 = count_bits(2),
    2 = count_bits(3),
    1 = count_bits(1024),
    2 = count_bits(1025),
    
    wall = acc(-1,0),
    wall = acc(0,-1),

    1 = min([3,2,1]),
    1 = min([1]),
    1 = min([unreachable, 2, 1, 3]),

    82 = do1(),
    138 = do2(),

    ok.