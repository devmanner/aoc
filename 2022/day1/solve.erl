-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, [0]).
parse_file(FD, [Prev|T]) ->
    case file:read_line(FD) of
        eof -> lists:reverse([Prev|T]);
        {ok, "\n"} ->
            parse_file(FD, [0,Prev|T]);
        {ok, Cals} ->
            Calories = list_to_integer(string:chomp(Cals)),
            parse_file(FD, [Prev+Calories|T])
    end.

do1() ->
    lists:max(solve:parse_file("input.txt")).

do2() ->
    [E1, E2, E3|_] = lists:sort(fun erlang:'>'/2, solve:parse_file("input.txt")),
    E1 + E2 + E3.

