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
blink([H|T]) ->
    case is_even(digits(H)) of
        true ->
            {High, Low} = split(H),
            [High, Low|blink(T)];
        false ->
            [H*2024|blink(T)]
    end.

blink(L, N) ->
    lists:foldl(fun(X, Acc) -> io:format("Blink: ~p ~p~n", [X, length(Acc)]), blink(Acc) end, L, lists:seq(1, N)).

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
    ok.

    %length(blink([773, 79858, 0, 71, 213357, 2937, 1, 3998391], 75)).
