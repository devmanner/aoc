-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    do_parse_file(FD).

do_parse_file(FD) ->
    case file:read_line(FD) of
        eof -> [];
        {ok, S} ->
            [Result|Operands] = re:split(string:chomp(S), "[:\\ ]+"),
            [{binary_to_integer(Result), lists:map(fun binary_to_integer/1, Operands)}|do_parse_file(FD)]
    end.

operators1() ->
    [
        fun(X, Y) -> X + Y end,
        fun(X, Y) -> X * Y end
    ].
operators2() ->
    [
        fun(X, Y) -> X + Y end,
        fun(X, Y) -> X * Y end,
        fun(X, Y) ->
            DigitsY = trunc(math:log10(Y)) + 1,
            X * trunc(math:pow(10, DigitsY)) + Y
        end
    ].

find(Result, Operands, Operators) ->
    find(Result, Operands, Operators, 0).
find(Result, [], _Operators, Acc) when Acc > Result ->
    false;
find(Result, [], _Operators, Acc) ->
    Acc == Result;
find(Result, [Op|Ops], Operators, Acc) ->
    R = lists:map(fun(F) -> find(Result, Ops, Operators, F(Acc, Op)) end, Operators),
    lists:member(true, lists:flatten(R)).

test(Fname, Operators) ->
    Eq = parse_file(Fname),
    True = lists:filter(fun({Res, Ops}) -> find(Res, Ops, Operators) end, Eq),
    lists:foldl(fun({Res, _}, Acc) -> Acc+Res end, 0, True).

test() ->
    true = find(19, [10, 9], operators1()),
    true = find(190, [10, 19], operators1()),
    true = find(220, [1, 21, 10], operators1()),
    
    3749 = test("test.txt", operators1()),
    1582598718861 = test("input.txt", operators1()),

    %% Part 2
    11387 = test("test.txt", operators2()),
    165278151522644 = test("input.txt", operators2()).
