-module(solve).
-compile(export_all).

ones(Len) ->
    lists:map(fun(_) -> 1 end, lists:seq(1, Len)).

next(A) ->
    A ++ [0] ++ lists:zipwith(fun(X, Y) -> X bxor Y end, lists:reverse(A), ones(length(A))).

checksum(X) ->
    C = checksum(X, []),
    case length(C) rem 2 of
        1 -> C;
        0 -> checksum(C)
    end.
checksum([], Acc) ->
    Acc;
checksum([X, X|T], Acc) ->
    checksum(T, [1|Acc]);
checksum([_, _|T], Acc) ->
    checksum(T, [0|Acc]).

fill(Init, Length) ->
    case length(Init) < Length of
        true ->
            fill(next(Init), Length);
        false ->
            Fill = lists:sublist(Init, Length),
            {Fill, checksum(Fill)}
    end.

print(L) ->
    lists:foreach(fun(X) -> io:format("~p", [X]) end, L),
    io:format("~n").

do1() ->
    {_, CS} = fill([0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,0,0], 272),
    print(CS),
    CS.

do2() ->
    {_, CS} = fill([0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,0,0], 35651584),
    print(CS),
    CS.

test() ->
    [1,0,0] = next([1]),
    [0,0,1] = next([0]),
    [1,1,1,1,1,0,0,0,0,0,0] = next([1,1,1,1,1]),
    [1,1,1,1,0,0,0,0,1,0,1,0,0,1,0,1,0,1,1,1,1,0,0,0,0] = next([1,1,1,1,0,0,0,0,1,0,1,0]),

    [1,0,0] = checksum([1,1,0,0,1,0,1,1,0,1,0,0]),

    {[1,0,0,0,0,0,1,1,1,1,0,0,1,0,0,0,0,1,1,1], [0,1,1,0,0]} = fill([1,0,0,0,0], 20),

    [1,0,0,1,1,0,1,0,0,1,0,0,1,0,0,1,0] = do1(),

    false = ([1,1,0,0,0,1,0,1,1,1,1,0,1,0,1,0,1] == do2()),

    ok.
