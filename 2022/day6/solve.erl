-module(solve).
-compile(nowarn_export_all).
-compile(export_all).



find_marker(S) ->
    find_marker(S, 4).

find_marker([B1, B2, B3, B4|_], N) when (B1 /= B2) and (B1 /= B3) and (B1 /= B4) and (B2 /= B3) and (B2 /= B4) and (B3 /= B4) ->
    N;
find_marker([_|T], N) ->
    find_marker(T, N+1).

find_message(S) ->
    find_message(S, 14).

find_message(S, N) ->
    case length(lists:usort(string:sub_string(S, 1, 14))) of
        14 ->
            N;
        _ ->
            [_|S2] = S,
            find_message(S2, N+1)
    end.

do1() ->
    {ok, Bin} = file:read_file("input.txt"),
    find_marker(erlang:binary_to_list(Bin)).

do2() ->
    {ok, Bin} = file:read_file("input.txt"),
    find_message(erlang:binary_to_list(Bin)).

test() ->
    7 = find_marker("mjqjpqmgbljsphdztnvjfqwrcgsmlb"),
    5 = find_marker("bvwbjplbgvbhsrlpgdmjqwftvncz"),
    6 = find_marker("nppdvjthqldpwncqszvftbrmjlhg"),
    10 = find_marker("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"),
    11 = find_marker("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"),
    
    1238 = do1(),


    19 = find_message("mjqjpqmgbljsphdztnvjfqwrcgsmlb"),
    23 = find_message("bvwbjplbgvbhsrlpgdmjqwftvncz"),
    23 = find_message("nppdvjthqldpwncqszvftbrmjlhg"),
    29 = find_message("nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"),
    26 = find_message("zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"),

    3037 = do2(),
    
    ok.

