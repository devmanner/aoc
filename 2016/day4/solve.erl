-module(solve).
-compile(export_all).

parse_file(Fname) ->
    {ok, FP} = file:open(Fname, [read]),
    do_parse_file(FP).
do_parse_file(FP) ->
    case file:read_line(FP) of
        eof -> [];
        {ok, Line} ->
            [parse(string:strip(Line, right, $\n)) | do_parse_file(FP)]
    end.

parse(S) ->
	[Checksum, Id | L] = lists:reverse(re:split(S, "[\\[\\]\\-]", [trim, {return, list}])),
    {lists:flatten(lists:join("-", lists:reverse(L))), list_to_integer(Id), Checksum}.

% Remove duplicates but preserve order
remove_dups([]) ->
    [];
remove_dups([H|T]) ->
    [H | [X || X <- remove_dups(T), X /= H]].

checksum({L, _, _}) ->
    Count = fun($-, {Cnt, M}) ->
                {Cnt, M};
            (X, {Cnt, M}) ->
                CntNew = maps:update_with(X, fun(Y) -> Y+1 end, 1, Cnt),
                {ok, N} = maps:find(X, CntNew),
                {CntNew, maps:update_with(N, fun(V) -> lists:sort([X|V]) end, [X], M)}
        end,
    {_, Freq} = lists:foldl(Count, {maps:new(), maps:new()}, L),

    AccCS = fun(X, Acc) ->
        case length(Acc) of
            5 ->
                Acc;
            _ -> 
                {ok, Candidates} = maps:find(X, Freq),
                lists:sublist(remove_dups(Acc ++ Candidates), 5)
        end
    end,
    lists:foldr(AccCS, [], lists:sort(maps:keys(Freq))).

check_checksum(Room={_, _, CS}) ->
    checksum(Room) == CS.

do1() ->
    L = parse_file("input1.txt"),
    Sum = fun(Room={_,X,_}, Acc) ->
        case check_checksum(Room) of
            true -> Acc + X;
            false -> Acc
        end
    end,
    lists:foldl(Sum, 0, L).

decrypt({L, ID, _CS}) ->
    F = fun($-) ->
            $ ;
        (X) ->
            ((X - $a + ID) rem ($z-$a+1)) + $a
    end,
    lists:map(F, L).

%% erl -pa . -eval "solve:do2(),erlang:halt()" | grep north
do2() ->
    L = parse_file("input1.txt"),
    F = fun(Room) ->
        io:format("~p ~s~n", [Room, decrypt(Room)])
    end,
    lists:foreach(F, L).

test() ->
    {"aaaaa-bbb-z-y-x", 123, "abxyz"} = parse("aaaaa-bbb-z-y-x-123[abxyz]"),
    {"a-b-c-d-e-f-g-h", 987, "abcde"} = parse("a-b-c-d-e-f-g-h-987[abcde]"),
    
    [1,2,3] = remove_dups([1,2,3]),
    [3,2,1] = remove_dups([3,3,2,1,1]),
    [a,b,c] = remove_dups([a,b,a,b,c,c,a]),
    [a,c,b] = remove_dups([a,c,a,c,a,b,a,b,c,c,a]),

    "fabcd" = checksum({"aaabbbcccdefffff", 1, 1}),
    "a" = checksum({"a", 1, 1}),
    "abxyz" = checksum(parse("aaaaa-bbb-z-y-x-123[abxyz]")),
    "abcde" = checksum(parse("a-b-c-d-e-f-g-h-987[abcde]")),
    "oarel" = checksum(parse("not-a-real-room-404[oarel]")),

    true = check_checksum(parse("aaaaa-bbb-z-y-x-123[abxyz]")),
    true = check_checksum(parse("a-b-c-d-e-f-g-h-987[abcde]")),
    true = check_checksum(parse("not-a-real-room-404[oarel]")),
    false = check_checksum(parse("totally-real-room-200[decoy]")),

    278221 = do1(),

    "very encrypted name" = decrypt(parse("qzmt-zixmtkozy-ivhz-343[zimth]")),

    ok.

