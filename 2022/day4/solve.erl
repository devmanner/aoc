-module(solve).
-compile(nowarn_export_all).
-compile(export_all).


parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, []).
parse_file(FD, Acc) ->
    case io:fread(FD, "", "~d-~d,~d-~d") of
        eof -> lists:reverse(Acc);
        {ok, [AFrom, ATo, BFrom, BTo]} ->
            % Assert assumptions
            true = AFrom =< ATo,
            true = BFrom =< BTo,            
            parse_file(FD, [{{AFrom, ATo}, {BFrom, BTo}}|Acc])
    end.

is_full_overlap({AFrom, ATo}, {BFrom, BTo}) when ((AFrom >= BFrom) and (ATo =< BTo)) or ((BFrom >= AFrom) and (BTo =< ATo)) ->
    true;
is_full_overlap(_, _) ->
    false.

overlap({AFrom, ATo}, {BFrom, BTo}) ->
    case 1 + min(ATo, BTo) - max(AFrom, BFrom) of
        N when N < 0 -> 0;
        N -> N
    end.

do1() ->
    Count = fun({A, B}, Acc) ->
        case is_full_overlap(A, B) of
            true -> Acc+1;
            false -> Acc
        end
    end,
    lists:foldl(Count, 0, parse_file("input.txt")).

do2() ->
    Count = fun({A, B}, Acc) ->
        case overlap(A, B) of
            0 -> Acc;
            _ -> Acc+1
        end
    end,
    lists:foldl(Count, 0, parse_file("input.txt")).


test() ->
    true = is_full_overlap({1,1}, {1,1}),
    true = is_full_overlap({1,2}, {1,2}),
    true = is_full_overlap({2,2}, {2,2}),
    true = is_full_overlap({2,3}, {1,4}),
    true = is_full_overlap({1,4}, {2,3}),
    true = is_full_overlap({2,3}, {2,4}),
    true = is_full_overlap({2,4}, {2,3}),
    
    false = is_full_overlap({2,3}, {4,5}),
    false = is_full_overlap({4,5}, {2,3}),
    false = is_full_overlap({2,4}, {3,5}),
    false = is_full_overlap({3,5}, {2,4}),

    424 = do1(),

    1 = overlap({5,7}, {7,9}),
    5 = overlap({2,8}, {3,7}),
    1 = overlap({6,6}, {4,6}),
    3 = overlap({2,6}, {4,8}),

    0 = overlap({1,1}, {5,5}),

    804 = do2(),    

    ok.