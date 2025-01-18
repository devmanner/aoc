-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    do_parse_file(FD).

do_parse_file(FD) ->
    {ok, [$B, $u, $t, $t, $o, $n, $ , $A, $:, $ |A]} = file:read_line(FD),
    {ok, [$B, $u, $t, $t, $o, $n, $ , $B, $:, $ |B]} = file:read_line(FD),
    {ok, [$P, $r, $i, $z, $e, $:, $ |Price]} = file:read_line(FD),

    ["X", AX, "Y", AY] = re:split(A, "[ ,+\n]+", [{return, list}, trim]),
    ["X", BX, "Y", BY] = re:split(B, "[ ,+\n]+", [{return, list}, trim]),
    ["X", PriceX, "Y", PriceY] = re:split(Price, "[ ,=\n]+", [{return, list}, trim]),

    Rest = case file:read_line(FD) of
        eof -> [];
        _ -> do_parse_file(FD)
    end,

    [{{list_to_integer(AX), list_to_integer(AY)}, {list_to_integer(BX), list_to_integer(BY)}, {list_to_integer(PriceX), list_to_integer(PriceY)}}|Rest].

is_int(Float) ->
    Tolerance = 0.0001,
    abs(Float - round(Float)) =< Tolerance.

solve({{AX, AY}, {BX, BY}, {PX, PY}}) ->
    % This is an equation system:
    % A * AX + B * BX = PX
    % A * AY + B * BY = PY
    %
    % Matrices:
    % AX BX   A  =  PX
    % AY BY   B     PY
    %
    % Written as Matrices:
    % A * B = C
    % 
    % Find A inverse and multiply by C
    Fact = 1 / ((AX * BY) - (AY * BX)),
    _AInverse = [[Fact * BY, Fact * -1*BX],
                [Fact * -1*AY, Fact * AX]],
    
    % Calculate AInverse * C
    [A, B] = [
                PX * Fact * BY + PY * Fact * -1*BX,
                PX * Fact * -1*AY + PY * Fact * AX
            ],

    case is_int(A) and is_int(B) of
        true ->
            % No solutions should have A or B higher than 100... Just checking...
            % Don't do it in stage 2, lol
            % true = ((A =< 100) and (B =< 100)),
            {round(A), round(B)};
        false ->
            no_discrete_solution
    end.

cost1(L) ->
    Fn = fun(X, Acc) ->
        case solve(X) of
            no_discrete_solution -> Acc;
            {A, B} -> Acc + 3*A + B
        end
    end,
    lists:foldl(Fn, 0, L).

cost2(L) ->
    Fn = fun({AXY, BXY, {PX, PY}}, Acc) ->
        X = {AXY, BXY, {PX+10000000000000, PY+10000000000000}},
        case solve(X) of
            no_discrete_solution -> Acc;
            {A, B} -> Acc + 3*A + B
        end
    end,
    lists:foldl(Fn, 0, L).

test() ->
    480 = cost1(parse_file("test1.txt")),
    37680 = cost1(parse_file("input.txt")),

    875318608908 = cost2(parse_file("test1.txt")),
    87550094242995 = cost2(parse_file("input.txt")),
    ok.