-module(solve).
-compile(export_all).

-define(SALT, "cuanljph").

shash(Salt, N) ->
    hashn(Salt, N, 2017).

hashn(Salt, N, Times) ->
    F = fun(_, Acc) ->
            string:lowercase(binary:encode_hex(crypto:hash(md5, Acc)))
        end,
    binary_to_list(lists:foldl(F, Salt ++ integer_to_list(N), lists:seq(1, Times))).

hash(Salt, N) ->
    b2h(crypto:hash(md5, Salt ++ integer_to_list(N))).

b2h(Bin) ->
    binary_to_list(string:lowercase(binary:encode_hex(Bin))).

classify(Hash) ->
    classify(Hash, none, 0, no_triple_found).
classify([], _, _, _) ->
    [];
classify([H|T], H, 2, no_triple_found) ->
    [{triple, H}|classify(T, H, 3, triple_found)];
classify([H|T], H, 4, triple_found) ->
    [{quintuple, H}|classify(T, H, 5, triple_found)];
classify([H|T], H, N, Mode) ->
    classify(T, H, N+1, Mode);
classify([H|T], _, _, Mode) ->
    classify(T, H, 1, Mode).


%% TMap maps: N -> Char
%% QMap maps: Char => ordset(N)
update(Salt, N, TMap, QMap, Algo) ->
    Hash = apply(?MODULE, Algo, [Salt, N]),
    L = classify(Hash),
    F = fun(X, Acc) ->
            maps:update_with(X, fun(Old) -> ordsets:add_element(N, Old) end, ordsets:from_list([N]), Acc)
        end,
    {
        lists:foldl(fun(X, Acc) -> maps:put(N, X, Acc) end, TMap, proplists:get_all_values(triple, L)),
        lists:foldl(F, QMap, proplists:get_all_values(quintuple, L))
    }.

update_range(Salt, NMin, NMax, InitTMap, InitQMap, Algo) ->
    F = fun(N, {TMap, QMap}) ->
        update(Salt, N, TMap, QMap, Algo)
    end,
    lists:foldl(F, {InitTMap, InitQMap}, lists:seq(NMin, NMax)).

is_key(N, TMap, QMap, Max) when N+1000 < Max ->
    case maps:get(N, TMap, not_found) of
        not_found -> false;
        Char ->
            Quintiples = lists:filter(fun(X) -> (X > N) and (X =< N+1000) end, maps:get(Char, QMap, [])),
            not ([] == Quintiples)
    end.

find_keys(Salt, Algo) ->
    Start = 0,
    Max = Start+1000,
    io:format("Init map: ~p to ~p~n", [Start, Max]),
    {TMap, QMap} = update_range(Salt, Start, Max, maps:new(), maps:new(), Algo),
    find_keys(Salt, Start, TMap, QMap, Max, [], Algo).
find_keys(_Salt, _N, _TMap, _QMap, _Max, Keys, _Algo) when length(Keys) >= 64 ->
    Keys;
find_keys(Salt, N, TMap, QMap, Max, Keys, Algo) when N+1000 >= Max ->
    io:format("Update range: ~p to ~p~n", [Max+1, Max*2]),
    {NewTMap, NewQMap} = solve:update_range(Salt, Max+1, Max*2, TMap, QMap, Algo),
    find_keys(Salt, N, NewTMap, NewQMap, Max*2, Keys, Algo);
find_keys(Salt, N, TMap, QMap, Max, Keys, Algo) ->
    case is_key(N, TMap, QMap, Max) of
        true -> find_keys(Salt, N+1, TMap, QMap, Max, [N|Keys], Algo);
        false -> find_keys(Salt, N+1, TMap, QMap, Max, Keys, Algo)
    end.

do1() ->
    hd(find_keys(?SALT, hash)).

do2() ->
    hd(find_keys(?SALT, shash)).

test() ->
    "0a" = b2h(<<0:4,10:4>>),
    "01" = b2h(<<0:4,1:4>>),

    [] = classify("abc123"),
    [{triple, $1}] = classify("abc111"),
    [{triple, $1}] = classify("111abc"),
    [{triple, $1}] = classify("abc1111"),
    [{triple, $1}] = classify("abc111abc"),

    [{triple, $1}, {quintuple, $1}] = classify("11111"),
    [{triple, $1}, {quintuple, $1}] = classify("abc11111abc"),

    [{triple, $1}, {quintuple, $1}, {quintuple, $3}] = classify("abc11111a222b33333abc"),

    "0034e0923cc38887a57bd7b1d4f953df" = hash("abc", 18),

    Max = 25000,
    {TMap, QMap} = solve:update_range("abc", 0, Max, maps:new(), maps:new(), hash),
    false = is_key(18, TMap, QMap, Max),
    true = is_key(39, TMap, QMap, Max),
    false = is_key(45, TMap, QMap, Max),
    false = is_key(64, TMap, QMap, Max),
    false = is_key(77, TMap, QMap, Max),
    false = is_key(79, TMap, QMap, Max),
    false = is_key(88, TMap, QMap, Max),
    false = is_key(91, TMap, QMap, Max),
    true = is_key(92, TMap, QMap, Max),
    true = is_key(22728, TMap, QMap, Max),

    22728 = hd(find_keys("abc", hash)),

    23769 = do1(),

    %% Part 2
    "577571be4de9dcce85a041ba0410f29f" = hashn("abc", 0, 1),
    "eec80a0c92dc8a0777c619d9bb51e910" = hashn("abc", 0, 2),
    "16062ce768787384c81fe17a7a60c7e3" = hashn("abc", 0, 3),
    "a107ff634856bb300138cac6568c0f24" = shash("abc", 0),

    % This is slow
    %20606 = do1(),

    ok.