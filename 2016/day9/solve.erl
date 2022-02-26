-module(solve).
-compile(export_all).

xx(X) ->
    {Xp, Len} = x(X),
    case len(X) == Len of
        true -> {Xp, Len};
        false -> xx(Xp)
    end.

x(X) ->
    R = x(X, []),
    {R, len(R)}.

x([], Acc) ->
    lists:reverse(Acc);
x([{Mult, S}|T], Acc) ->
    x(T, [{Mult, x(S, [])}|Acc]);
x([$(|Tail], Acc) ->
    {ok, [Chars, Mult], Rest} = io_lib:fread("~dx~d)", Tail),
    x(lists:nthtail(Chars, Rest), [{Mult, lists:sublist(Rest, Chars)}|Acc]);
x([H|T], Acc) ->
   x(T, [H|Acc]).

len([]) ->
    0;
len([{X, S}|T]) ->
    X * len(S) + len(T);
len([_|T]) ->
    1 + len(T).

do1() ->
    {ok, FD} = file:open("input1.txt", [read]),
    {ok, X} = file:read_line(FD),
    {_, Len} = x(string:strip(X, right, $\n)),
    Len.

do2() ->
    {ok, FD} = file:open("input1.txt", [read]),
    {ok, X} = file:read_line(FD),
    {_, Len} = xx(string:strip(X, right, $\n)),
    Len.


test() ->
    {"ADVENT", 6} = x("ADVENT"),
    {[$A,{5,"B"},$C],7} = x("A(1x5)BC"),
    {[{3,"XYZ"}],9} = x("(3x3)XYZ"),
    {[$A,{2,"BC"},$D,{2,"EF"},$G],11} = x("A(2x2)BCD(2x2)EFG"),
    {[{1,"(1x3)A"}],6} = x("(6x1)(1x3)A"),
    {[$X,{2,"(3x3)ABC"},$Y],18} = x("X(8x2)(3x3)ABCY"),

    {[{3,"XYZ"}],9} = xx("(3x3)XYZ"),
    {[$X,{2,[{3,"ABC"}]},$Y],20} = xx("X(8x2)(3x3)ABCY"),
    {[{12,[{12,[{14,[{10,[{12,"A"}]}]}]}]}],241920} = xx("(27x12)(20x12)(13x14)(7x10)(1x12)A"),
    {[{3,[{3,"ABC"},{3,"XY"},{2,"PQRST"}]},$X,{9,[{2,"TWO"},{7,"SEVEN"}]}],445}
        = xx("(25x3)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"),

    % Something that takes "a lot of time" if xx is poorly implemented
    Long = lists:foldl(fun(_, Acc) -> ["(25x100)(3x3)ABC(2x3)XY(5x2)PQRSTX(18x9)(3x2)TWO(5x7)SEVEN"|Acc] end, [], lists:seq(1,10000)),
    {Time, _} = timer:tc(?MODULE, xx, [Long]),

    Time.

