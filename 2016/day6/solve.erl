-module(solve).
-compile(export_all).

do1(Fname) ->
    doit(Fname, fun(X, Y) -> X > Y end, 0).
do2(Fname) ->
    doit(Fname, fun(X, Y) -> X < Y end, 99999999).

doit(Fname, Op, Init) ->
    {ok, FD} = file:open(Fname, [read]),
    Maps = read_and_count(FD, []),
    MinMax = fun(Key, Value, {MaxKey, MaxVal}) -> 
            case Op(Value, MaxVal) of
                true ->
                    {Key,Value};
                false ->
                    {MaxKey, MaxVal}
             end
         end,
    R = lists:map(fun(M) -> maps:fold(MinMax, {nothing, Init}, M) end, Maps),
    lists:flatten(lists:map(fun({C, _}) -> C end, R)).

read_and_count(FD, Maps) ->
    case file:read_line(FD) of
        {ok, L} ->
            Line = re:split(string:strip(L, right, $\n), "", [trim, {return, list}]),
            Count = fun(Map, Char) ->
                    maps:update_with(Char, fun(X) -> X+1 end, 1, Map)
                end,
            read_and_count(FD, lists:zipwith(Count, init_maps(Maps, length(Line)), Line));
        eof ->
            Maps
    end.

init_maps([], N) ->
    lists:map(fun(_) -> maps:new() end, lists:seq(1, N));
init_maps(M, _) ->
    M.

