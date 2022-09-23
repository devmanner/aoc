-module(solve).
-compile(export_all).

-define(INPUT, "^.^^^.^..^....^^....^^^^.^^.^...^^.^.^^.^^.^^..^.^...^.^..^.^^.^..^.....^^^.^.^^^..^^...^^^...^...^.").

next(S) ->
    next("." ++ S ++ ".", []).

next([_,_], Acc) ->
    lists:reverse(Acc);
% Its left and center tiles are traps, but its right tile is not.
next([$^,$^,$.|T], Acc) ->
    next([$^,$.|T], [$^|Acc]);
% Its center and right tiles are traps, but its left tile is not.
next([$.,$^,$^|T], Acc) ->
    next([$^,$^|T], [$^|Acc]);
% Only its left tile is a trap.
next([$^,$.,$.|T], Acc) ->
    next([$.,$.|T], [$^|Acc]);
% Only its right tile is a trap.
next([$.,$.,$^|T], Acc) ->
    next([$.,$^|T], [$^|Acc]);
% In any other situation, the new tile is safe.
next([_,C,R|T], Acc) ->
    next([C,R|T], [$.|Acc]).

n_safe_tiles(S) ->
    IsSafe = fun(X) -> X == $. end,
    length(lists:filter(IsSafe, S)).
n_safe_tiles(N, S) ->
    F = fun(_, {SafeTiles, Prev}) ->
        Next = next(Prev),
        {SafeTiles + n_safe_tiles(Next), Next}
    end,
    lists:foldl(F, {n_safe_tiles(S), S}, lists:seq(1, N-1)).

do1() ->
    n_safe_tiles(40, ?INPUT).

do2() ->
    n_safe_tiles(400000, ?INPUT).

% Calculate all the rows up to N. Mainly for testing.
rows(N, S) ->
    lists:reverse(lists:foldl(fun(_N, [Prev|Rest]) -> [next(Prev),Prev|Rest] end, [S], lists:seq(1, N-1))).

test() ->
    [
        "..^^.",
        ".^^^^",
        "^^..^"
    ] = rows(3, "..^^."),

    [
        ".^^.^.^^^^",
        "^^^...^..^",
        "^.^^.^.^^.",
        "..^^...^^^",
        ".^^^^.^^.^",
        "^^..^.^^..",
        "^^^^..^^^.",
        "^..^^^^.^^",
        ".^^^..^.^^",
        "^^.^^^..^^"
    ] = rows(10, ".^^.^.^^^^"),

    {1926, _} = do1(),
    {19986699, _} = do2(),
    
    ok.