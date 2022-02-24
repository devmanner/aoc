-module(solve).
-compile(export_all).


parse_input(Fname) ->
	{ok, FP} = file:open(Fname, [read]),
	do_parse_input(FP).
do_parse_input(FP) ->
	case file:read_line(FP) of
		{ok, Line} ->
			{ok, L, "\n"} = io_lib:fread("~d ~d ~d", Line),
			[L | do_parse_input(FP)];
		eof ->
			[]
	end.

is_triangle([A, B, C]) ->
	(A + B > C) and (A + C > B) and (B + C > A).

t1() ->
	L = parse_input("input1.txt"),
	length(lists:filter(fun(X) -> is_triangle(X) end, L)).

t2() ->
	L = parse_input("input1.txt"),
	length(t2(L)).
t2([]) ->
	[];
t2([[A1, B1, C1], [A2, B2, C2], [A3, B3, C3] | T]) ->
	L = lists:filter(fun(X) -> is_triangle(X) end, [[A1, A2, A3], [B1, B2, B3], [C1, C2, C3]]),
	L ++ t2(T).

