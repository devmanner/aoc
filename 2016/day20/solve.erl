-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, []).
parse_file(FD, Acc) ->
    case io:fread(FD, "", "~d-~d") of
        {ok, [From, To]} ->
            parse_file(FD, [{From, To}|Acc]);
        eof ->
            lists:sort(Acc)
    end.

max_span(N, L) ->
    max_span(N, {-1, -1}, L).

% Found no span
max_span(_N, {-1, -1}, []) ->
    not_found;
% Reached the end. Returning the best span
max_span(_N, {BestFrom, BestTo}, []) ->
    {BestFrom, BestTo};
% Found a span that starts before N and ends after N and overlaps the one we already have
max_span(N, {_BestFrom, BestTo}, [{From, To}|T]) when (From =< N) and (To >= N) and (To > BestTo) ->
    max_span(N, {From, To}, T);
% The rest of the spans in the list will not match because they start too late
max_span(N, {-1, -1}, [{From, _To}|_T]) when (From > N) ->
    not_found;
% The rest of the spans in the list will not match, returning the best
max_span(N, {BestFrom, BestTo}, [{From, _To}|_T]) when From > N ->
    {BestFrom, BestTo};
max_span(N, {BestFrom, BestTo}, [{_From, _To}|T]) ->
    max_span(N, {BestFrom, BestTo}, T).

do1() ->
    [{0, X}|L] = parse_file("input.txt"),
    do1(X, L).
do1(X, L) ->
    case max_span(X, L) of
        not_found -> X;
        {_From,To} ->
            do1(To+1, L)
    end.

% Level 2 implementation is really naive. Works as long as non blacklisted IPs are few...
do2() ->
    [{0, X}|L] = parse_file("input.txt"),
    do2(0, X, L).
do2(Cnt, X, _L) when X > 4294967295 ->
    Cnt;
do2(Cnt, X, L) ->
    case max_span(X, L) of
        not_found ->
            do2(Cnt+1, X+1, L);
        {_From,To} ->
            do2(Cnt, To+1, L)
    end.

test() ->
    L = lists:sort([
            {2, 3},
            {1, 4},
            {3, 5}
        ]),

    {1,4} = max_span(1, L),
    {3,5} = max_span(3, L),
    not_found = max_span(6, L),
    
    17348574 = do1(),

    104 = do2(),

    ok.
