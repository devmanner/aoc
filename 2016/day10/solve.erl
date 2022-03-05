-module(solve).
-compile(export_all).


parse_file(Fname) -> 
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, maps:new()).
parse_file(FD, Map) ->
    case file:read_line(FD) of
        {ok, [$v, $a, $l, $u, $e|_]=Line} ->
            {ok, [Value, Bot], _} = io_lib:fread("value ~d goes to bot ~d", Line),
            parse_file(FD, maps:update_with({bot, Bot}, fun({S, Low, High}) -> {ordsets:add_element(Value, S), Low, High} end, {ordsets:from_list([Value]), undef, undef}, Map));
        {ok, [$b, $o, $t|_]=Line} ->
            {ok, [Bot, Low, LowNum, High, HighNum], _} = io_lib:fread("bot ~d gives low to ~s ~d and high to ~s ~d", Line),
            parse_file(FD, maps:update_with({bot, Bot}, fun({S, undef, undef}) -> {S, {list_to_atom(Low), LowNum}, {list_to_atom(High), HighNum}} end, {ordsets:new(), {list_to_atom(Low), LowNum}, {list_to_atom(High), HighNum}}, Map));
        eof ->
            Map
    end.


simulate(Map, N) ->
    lists:foldl(fun(_, Acc) -> solve:simulate(Acc) end, Map, lists:seq(1, N)).

simulate(Map) ->
    F = fun({bot, Bot}, {[L, H], {Low, LowNum}, {High, HighNum}}, Acc) ->
            case (L == 17) and (H == 61) of
                true -> io:format("Found Bot: ~p~n", [Bot]);
                false -> ok
            end,
            Acc2 = maps:update_with({Low, LowNum}, fun({S, X, Y}) -> {ordsets:add_element(L, S), X, Y} end, ordsets:from_list([L]), Acc),
            Acc3 = maps:update_with({High, HighNum}, fun({S, X, Y}) -> {ordsets:add_element(H, S), X, Y} end, ordsets:from_list([H]), Acc2),
            maps:update({bot, Bot}, {ordsets:new(), {Low, LowNum}, {High, HighNum}}, Acc3);
        (_Bot, _Value, Acc) ->
            Acc    
        end,
    maps:fold(F, Map, Map).

do1() ->
    simulate(solve:parse_file("input1.txt"), 30).

do2() ->
    M = simulate(solve:parse_file("input1.txt"), 300),
    hd(maps:get({output, 0}, M)) * hd(maps:get({output, 1}, M)) * hd(maps:get({output, 2}, M)).