-module(solve).
-compile(export_all).

lbxor([], []) ->  [];
lbxor([H1|T1], [H2|T2]) ->
    [H1 bxor H2|lbxor(T1, T2)].

ones(Len) ->
    lists:map(fun(_) -> 1 end, lists:seq(1, Len)).

next(A) ->
    B = lists:reverse(A),
    A ++ [0] ++ lbxor(B,  ones(length(A))).

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

do1() ->
    {_, CS} = fill([0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,0,0], 272),
    CS.

do2() ->
    {_, CS} = fill([0,0,1,1,1,1,0,1,1,1,1,1,0,1,0,0,0], 35651584),
    CS.

test() ->
    [1] = lbxor([1], [0]),
    [1,1] = lbxor([1,0], [0,1]),


    [1,0,0] = next([1]),
    [0,0,1] = next([0]),
    [1,1,1,1,1,0,0,0,0,0,0] = next([1,1,1,1,1]),
    [1,1,1,1,0,0,0,0,1,0,1,0,0,1,0,1,0,1,1,1,1,0,0,0,0] = next([1,1,1,1,0,0,0,0,1,0,1,0]),

    [1,0,0] = checksum([1,1,0,0,1,0,1,1,0,1,0,0]),

    {[1,0,0,0,0,0,1,1,1,1,0,0,1,0,0,0,0,1,1,1], [0,1,1,0,0]} = fill([1,0,0,0,0], 20),

    [1,0,0,1,1,0,1,0,0,1,0,0,1,0,0,1,0] = do1(),

    ok.
