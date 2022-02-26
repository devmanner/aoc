-module(solve).
-compile(export_all).

xx(X) ->
    Xp = x(X),
    io:format("~p ~p~n", [length(X), length(Xp)]),
    case length(Xp) == length(X) of
        true -> X;
        false -> xx(Xp)
    end.

x(X) ->
    x(X, []).

x([], Acc) ->
    lists:flatten(lists:reverse(Acc));
x([$(|Tail], Acc) ->
    {ok, [Chars, Mult], Rest} = io_lib:fread("~dx~d)", Tail),
    x(lists:nthtail(Chars, Rest), [lists:duplicate(Mult, lists:sublist(Rest, Chars))|Acc]);
x([H|T], Acc) ->
   x(T, [H|Acc]).

do1() ->
    {ok, FD} = file:open("input1.txt", [read]),
    {ok, X} = file:read_line(FD),
    length(x(string:strip(X, right, $\n))).

do2() ->
    {ok, FD} = file:open("input1.txt", [read]),
    {ok, X} = file:read_line(FD),
    length(xx(string:strip(X, right, $\n))).

test() ->
    "ADVENT" = x("ADVENT"),
    "ABBBBBC" = x("A(1x5)BC"),
    "XYZXYZXYZ" = x("(3x3)XYZ"),
    "ABCBCDEFEFG" = x("A(2x2)BCD(2x2)EFG"),
    "(1x3)A" = x("(6x1)(1x3)A"),
    "X(3x3)ABC(3x3)ABCY" = x("X(8x2)(3x3)ABCY"),

    "XYZXYZXYZ" = xx("(3x3)XYZ"),
    "XABCABCABCABCABCABCY" = xx("X(8x2)(3x3)ABCY"),
    241920 = length(xx("(27x12)(20x12)(13x14)(7x10)(1x12)A")),
    445 = length(xx("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN")),

    Long = lists:foldl(fun(_, Acc) -> ["(25x100)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"|Acc] end, [], lists:seq(1,10000)),
    {Time, _} = timer:tc(?MODULE, xx, [Long]),

    Time.

