-module(solve).
-compile(nowarn_export_all).
-compile(export_all).


parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    Piles = parse_piles(FD, [], 0),
    Cmds = parse_commands(FD, []),
    {Piles, Cmds}.
parse_piles(FD, Stacks, N) ->    
    case file:read_line(FD) of
        % The last line of the piles section
        {ok, [$ , $1|_]} ->
            % Read the empty line and discard it.
            {ok, _} = file:read_line(FD),
            lists:map(fun(L) -> lists:reverse(lists:filter(fun(X) -> X /= $. end, L)) end, Stacks);
        {ok, Data} ->
            Data2 = re:replace(Data, "\\ {4}", ".", [{return, list}, global]),
            Data3 = re:replace(Data2, "[\\[\\]\\ \n]", "", [{return, list}, global]),
            case Stacks of
                [] ->
                    parse_piles(FD, lists:map(fun(X) -> [X] end, Data3), N+1);
                _ ->
                    parse_piles(FD, lists:zipwith(fun(A, B) -> [A|B] end, Data3, Stacks), N+1)
            end
    end.

parse_commands(FD, Cmds) ->
    case io:fread(FD, "", "move ~d from ~d to ~d") of
        {ok, [N, From, To]} ->
            parse_commands(FD, [{N, From, To}|Cmds]);
        eof ->
            lists:reverse(Cmds)
    end.

pop(1, [Pile|Piles], ToPop) ->
    {Popped, NewPile} = lists:split(ToPop, Pile),
    {Popped, [NewPile|Piles]};
pop(Nth, [Pile|Piles], ToPop) ->
    {Popped, NewPiles} = pop(Nth-1, Piles, ToPop),
    {Popped, [Pile|NewPiles]}.

push(1, [Pile|Piles], ToPush) ->
    [ToPush ++ Pile|Piles];
push(Nth, [Pile|Piles], ToPush) ->
    [Pile|push(Nth-1, Piles, ToPush)].

crate_mover9000(X) -> lists:reverse(X).
crate_mover9001(X) -> X.

exec_commands(_, Piles, []) ->
    Piles;
exec_commands(Model, Piles, [{N, From, To}|T]) ->
    {Popped, Piles2} = pop(From, Piles, N),
    Piles3 = push(To, Piles2, apply(?MODULE, Model, [Popped])),
    exec_commands(Model, Piles3, T).

do1() ->
    {Piles, Cmds} = parse_file("input.txt"),
    PilesOut = exec_commands(crate_mover9000, Piles, Cmds),
    lists:foldr(fun([X|_], Acc) -> [X|Acc] end, [], PilesOut).

do2() ->
    {Piles, Cmds} = parse_file("input.txt"),
    PilesOut = exec_commands(crate_mover9001, Piles, Cmds),
    lists:foldr(fun([X|_], Acc) -> [X|Acc] end, [], PilesOut).

test() ->
    [[1,2],[1,2,3],[0,1]] = solve:push(3, [[1,2],[1,2,3],[1]], [0]),
    [[1,2],[1,2,3],[0,1,2]] = solve:push(3, [[1,2],[1,2,3],[2]], [0,1]),

    {[0], [[1,2],[1,2,3],[1]]} = solve:pop(3, [[1,2],[1,2,3],[0,1]], 1),
    {[0,1], [[1,2],[1,2,3],[2]]} = solve:pop(3, [[1,2],[1,2,3],[0,1,2]], 2),

    {Piles, _Cmds} = parse_file("test_input.txt"),

    ["NZ", "DCM", "P"] = Piles,

    Piles2 = exec_commands(crate_mover9000, Piles, [{1, 2, 1}]),
    ["DNZ", "CM", "P"] = Piles2,

    Piles3 = exec_commands(crate_mover9000, Piles2, [{3, 1, 3}]),
    ["", "CM", "ZNDP"] = Piles3,

    Piles4 = exec_commands(crate_mover9000, Piles3, [{2, 2, 1}]),
    ["MC", "", "ZNDP"] = Piles4,
    
    Piles5 = exec_commands(crate_mover9000, Piles4, [{1, 1, 2}]),
    ["C", "M", "ZNDP"] = Piles5,

    "TQRFCBSJJ" = do1(),

    "RMHFJNVFP" = do2(),

    ok.