-module(solve).
-compile(nowarn_export_all).
-compile(export_all).


digits(0) ->
    1;
digits(N) when N > 0 ->
    trunc(math:log10(N)) + 1.

is_even(N) ->
    N band 1 /= 1.

split(N) ->
    Half = digits(N) div 2,
    Divider = math:pow(10, Half),
    LeftPart = N div trunc(Divider),
    RightPart = N rem trunc(Divider),
    {LeftPart, RightPart}.

blink([]) ->
    [];
blink([0|T]) ->
    [1|blink(T)];
blink([H|T]) when is_integer(H) ->
    case is_even(digits(H)) of
        true ->
            {High, Low} = split(H),
            [High, Low|blink(T)];
        false ->
            [H*2024|blink(T)]
    end;
% For stage 2 we add support for Cntlists
blink([{0, N}|T]) ->
    [{1, N}|blink(T)];
blink([{H, N}|T]) ->
    case is_even(digits(H)) of
        true ->
            {High, Low} = split(H),
            [{High, N}, {Low, N}|blink(T)];
        false ->
            [{H*2024, N}|blink(T)]
    end.

blink(L, N) ->
    Fn = fun(_X, Acc) ->
        blink(Acc)
    end,
    lists:foldl(Fn, L, lists:seq(1, N)).

list_to_cntlist(L) ->
    compact_cntlist(lists:map(fun(X) when is_integer(X) -> {X, 1} end, L)).

compact_cntlist([{_,_}|_]=Cntlist) ->
    F = fun({X, N1}, Acc) ->
        case hd(Acc) of
            {X, N2} -> [{X, N1+N2}|tl(Acc)];
            _ -> [{X, N1}|Acc]
        end 
    end,
    L = lists:sort(Cntlist),
    lists:reverse(lists:foldl(F, [hd(L)], tl(L))).

length_after_blinks(L, N) ->
    lab(list_to_cntlist(L), N).
lab(L, 0) ->
    lists:foldl(fun({_X, N}, Acc) -> Acc + N end, 0, L);
lab(L, N) ->
    lab(compact_cntlist(blink(L)), N-1).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test() ->
    3 = digits(100),
    2 = digits(99),
    3 = digits(101),
    1 = digits(1),
    2 = digits(10),

    true = is_even(2),
    false = is_even(1),

    {10, 10} = split(1010),
    {1, 1} = split(11),
    {10, 0} = split(1000),
    {1, 0} = split(10),
    
    [253000, 1, 7] = blink([125, 17], 1),
    [253, 0, 2024, 14168] = blink([125, 17], 2),
    [512072, 1, 20, 24, 28676032] = blink([125, 17], 3),
    [512, 72, 2024, 2, 0, 2, 4, 2867, 6032] = blink([125, 17], 4),
    [1036288, 7, 2, 20, 24, 4048, 1, 4048, 8096, 28, 67, 60, 32] = blink([125, 17], 5),
    [2097446912, 14168, 4048, 2, 0, 2, 4, 40, 48, 2024, 40, 48, 80, 96, 2, 8, 6, 7, 6, 0, 3, 2] = blink([125, 17], 6),
    55312 = length(blink([125, 17], 25)),


    199982 = length(blink([773, 79858, 0, 71, 213357, 2937, 1, 3998391], 25)),

    %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    [{1, 3}, {2, 5}, {3, 1}] = list_to_cntlist([3,2,2,2,1,2,2,1,1]),

    [{1, 4}, {2, 1}] = compact_cntlist([{1,1}, {2,1}, {1,1}, {1,2}]),

    F = fun(Blinks) ->
        Stones = [125, 17],
        true = length_after_blinks(Stones, Blinks) == length(blink(Stones, Blinks))
    end,
    lists:foreach(F, lists:seq(1, 20)),


    199982 = length_after_blinks([773, 79858, 0, 71, 213357, 2937, 1, 3998391], 25),
    237149922829154 = length_after_blinks([773, 79858, 0, 71, 213357, 2937, 1, 3998391], 75),

    ok.
