-module(solve).
-compile(export_all).


do1(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    Maps = do1(FD, []),
    F = fun(Key, Value, {MaxKey, MaxVal}) -> 
            case Value > MaxVal of
                true ->
                    {Key,Value};
                false ->
                    {MaxKey, MaxVal}
             end
         end,
    lists:flatten(lists:map(fun({C, _}) -> C end, lists:map(fun(M) -> maps:fold(F, {nothing, 0}, M) end, Maps))).

init_maps([], N) ->
    lists:map(fun(_) -> maps:new() end, lists:seq(1, N));
init_maps(M, _) ->
    M.

do1(FD, Maps) ->
    case file:read_line(FD) of
        {ok, L} ->
            Line = re:split(string:strip(L, right, $\n), "", [trim, {return, list}]),
            F = fun(Map, Char) ->
                    maps:update_with(Char, fun(X) -> X+1 end, 1, Map)
                end,
            do1(FD, lists:zipwith(F, init_maps(Maps, length(Line)), Line));
        eof ->
            Maps
    end.



