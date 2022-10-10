-module(solve).
-export([test/0]).
-export([do1/0, do2/0]).
-export([fill/2]).

-define(INIT_STATE, [0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,0,0]).

checksum(X) ->
    C = checksum(X, []),
    case length(C) band 1 of
        1 -> C;
        0 -> checksum(C)
    end.

checksum([] , Acc) ->
    lists:reverse(Acc);
checksum([X, Y|T], Acc) ->
    % We are setting it to 1 if X and Y are the same
    % bnot 0 is not 1, (0xfffffe) that is why we band it with 1
    checksum(T, [(bnot (X bxor Y)) band 1|Acc]).

next(A) ->
    lists:concat([A, [0], lists:reverse(lists:map(fun(X) -> X bxor 1 end, A))]).

fill(List, Length) when length(List) < Length ->
    fill(next(List), Length);
fill(List, Length) ->
    Fill = lists:sublist(List, Length),
    {Fill, checksum(Fill)}.

print(L) ->
    lists:foreach(fun(X) -> io:format("~p", [X]) end, L),
    io:format("~n").

do1() ->
    Fun = fun() ->
        {_, CS} = fill(?INIT_STATE, 272),
        print(CS),
        CS
    end,
    {Time, Ret} = timer:tc(Fun),
    io:format("Test took: ~p ms~n", [Time/1000]),
    Ret.

do2() ->
    Fun = fun() ->
        {_, CS} = fill(?INIT_STATE, 35651584),
        print(CS),
        CS
    end,
    {Time, Ret} = timer:tc(Fun),
    io:format("Test took: ~p ms~n", [Time/1000]),
    Ret.

test() ->
    [1,0,0] = next([1]),
    [0,0,1] = next([0]),
    [1,1,1,1,1,0,0,0,0,0,0] = next([1,1,1,1,1]),
    [1,1,1,1,0,0,0,0,1,0,1,0,0,1,0,1,0,1,1,1,1,0,0,0,0] = next([1,1,1,1,0,0,0,0,1,0,1,0]),

    [1] = checksum([0, 0]),
    [1] = checksum([1, 1]),
    [0] = checksum([0, 1]),
    [0] = checksum([1, 0]),
    [1,0,0] = checksum([1,1,0,0,1,0,1,1,0,1,0,0]),

    {[1,0,0,0,0,0,1,1,1,1,0,0,1,0,0,0,0,1,1,1], [0,1,1,0,0]} = fill([1,0,0,0,0], 20),

    [1,0,0,1,1,0,1,0,0,1,0,0,1,0,0,1,0] = do1(),

    [1,0,1,0,1,0,1,1,1,1,0,1,0,0,0,1,1] = do2(),

    ok.
