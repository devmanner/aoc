-module(solve).

-compile(export_all).

m1() ->
	[
		[oob, oob, oob, oob, oob],
		[oob,   1,   2,   3, oob],
		[oob,   4,   5,   6, oob],
		[oob,   7,   8,   9, oob],
		[oob, oob, oob, oob, oob]
	].


m2() ->
	[
		[oob, oob, oob, oob, oob, oob, oob],
		[oob, oob, oob,   1, oob, oob, oob],
		[oob, oob,   2,   3,   4, oob, oob],
		[oob,   5,   6,   7,   8,   9, oob],
		[oob,  oob, $A,  $B,  $C, oob, oob],
		[oob, oob, oob,  $D, oob, oob, oob],
		[oob, oob, oob, oob, oob, oob, oob]
	].

acc({R, C}, M) ->
	lists:nth(C+1, lists:nth(R+1, M)).

move({R, C}, $U) -> {R-1, C};
move({R, C}, $D) -> {R+1, C};
move({R, C}, $L) -> {R, C-1};
move({R, C}, $R) -> {R, C+1}.

move({R, C}, [], _) ->
	{R, C};
move({R, C}, [Dir|T], M) ->
	case acc(move({R, C}, Dir), M) of
		oob -> move({R, C}, T, M);
		_ -> move(move({R, C}, Dir), T, M)
	end.

doit({StartRow, StartCol}, L, M) ->
	F = fun(Moves, [{R, C}|T]) ->
			[move({R, C}, Moves, M), {R, C} | T]
		end,
	Res = lists:foldl(F, [{StartRow, StartCol}], L),
	lists:map(fun(X) -> acc(X, M) end, lists:reverse(lists:sublist(Res, length(Res)-1))).


parse_input(Fname) ->
	{ok, FP} = file:open(Fname, [read]),
	do_parse_input(FP).
do_parse_input(FP) ->
	case file:read_line(FP) of
		{ok, Line} ->
			[string:strip(Line, right, $\n) | do_parse_input(FP)];
		eof ->
			[]
	end.

test() ->
	oob = acc({0, 0}, m1()),
	5 = acc({2, 2}, m1()),
	8 = acc({3, 2}, m1()),


	{1, 1} = move({1, 1}, [$U], m1()),
	{1, 1} = move({1, 1}, [$L], m1()),
	{2, 1} = move({1, 1}, [$D], m1()),
	{1, 2} = move({1, 1}, [$R], m1()),


	[1,9,8,5] = doit({2,2}, parse_input("input.txt"), m1()),

	[2, 4, 8, 6, 2] = doit({2,2}, parse_input("input1.txt"), m1()),

	[4, 6, $C, 9, 1] = doit({3,1}, parse_input("input1.txt"), m2()),	

	ok.

