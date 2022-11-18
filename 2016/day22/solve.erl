-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

%% {Orddict({X, Y} -> {Capacity, Used}), proplist(Free -> [{X, Y}, ...])}
create_map(L) ->
    M = orddict:from_list(L),
    GenProplist = fun({{X, Y}, {Cap, Used}}, Acc) ->
        Free = Cap - Used,
        case lists:keyfind(Free, 1, Acc) of
            false ->
                lists:sort(fun erlang:'>'/2, [{Free, [{X, Y}]}|Acc]);
            {Free, Found} ->
                lists:sort(fun erlang:'>'/2, [{Free, [{X, Y}|Found]}|lists:keydelete(Free, 1, Acc)])
        end
    end,
    {M, lists:foldl(GenProplist, [], L)}.

nvp({PosMap, FreeMap}) ->
    FindVP = fun({X, Y}, {_Cap, Used}, Acc) ->
        AllViablePairs = fun({Free, _Pairs}) ->
            (Free >= Used)
        end,
        VP = lists:takewhile(AllViablePairs, FreeMap),
        Res = lists:foldl(fun({_, Pairs}, Acc2) -> lists:map(fun(P) -> {{X, Y}, P} end, Pairs) ++ Acc2 end, Acc, VP),
        Res
    end,
    % May contain pairs of X,Y -> X,Y
    Unfiltered = orddict:fold(FindVP, [], PosMap),
    % ... so we filter those out...
    FilterEqual = fun({{X, Y}, {X, Y}}) -> false;
                    (_) -> true
                end,
    Filtered = lists:filter(FilterEqual, Unfiltered),
    % ... and we also filter out the empty ones.
    FilterEmpty = fun({A, _B}) ->
        {ok, {_Cap, Used}} = orddict:find(A, PosMap),
        Used /= 0
    end,
    lists:filter(FilterEmpty, Filtered).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    {ok,"root@ebhq-gridcenter# df -h\n"} = file:read_line(FD),
    {ok,"Filesystem              Size  Used  Avail  Use%\n"} = file:read_line(FD),
    parse_file(FD, []).
parse_file(FD, Acc) ->
    case file:read_line(FD) of
        eof ->
            Acc;
        {ok, Line} ->
            {ok, [X, Y, Size, Used, _Avail, _Use], _} = io_lib:fread("/dev/grid/node-x~d-y~d     ~dT   ~dT    ~dT   ~d%", Line),
            parse_file(FD, [{{X, Y}, {Size, Used}}|Acc])
    end.

do1() ->
    NVP = nvp(create_map(parse_file("input.txt"))),
    length(NVP).

test() ->
    [{{0,2},{0,0}},{{0,1},{0,0}},{{0,0},{0,2}},{{0,0},{0,1}}] = solve:nvp(solve:create_map(solve:parse_file("input_test.txt"))),

    {PosMap, FreeMap} = create_map([
        {{0, 0}, {100, 0}},{{1, 0}, {100, 0}},{{2, 0}, {100, 10}},
        {{0, 1}, {100, 10}},{{1, 1}, {100, 50}},{{2, 1}, {100, 10}},
        {{0, 2}, {100, 50}},{{1, 2}, {100, 40}},{{2, 2}, {100, 10}}
    ]),
    nvp({PosMap, FreeMap}),

    864 = do1(),

    ok.



