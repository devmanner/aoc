-module(level1).

-compile(export_all).

stop(-1) -> 0;
stop(3) -> 2;
stop(X) -> X.

move({R, C}, []) ->
	{R, C};
move({R, C}, [$U|T]) ->
	move({stop(R-1), C}, T);
move({R, C}, [$D|T]) ->
	move({stop(R+1), C}, T);
move({R, C}, [$L|T]) ->
	move({R, stop(C-1)}, T);
move({R, C}, [$R|T]) ->
	move({R, stop(C+1)}, T).

rc_to_n({R, C}) ->
	1+R*3+C.

doit({StartRow, StartCol}, L) ->
	F = fun(Moves, [{R, C}|T]) ->
			[move({R, C}, Moves), {R, C} | T]
		end,
	Res = lists:foldl(F, [{StartRow, StartCol}], L),
	lists:map(fun(X) -> rc_to_n(X) end, lists:reverse(lists:sublist(Res, length(Res)-1))).

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
	{0, 1} = aoc:move({1,1}, "U"),
	{0, 1} = aoc:move({1,1}, "UU"),

	1 = rc_to_n({0,0}),
	4 = rc_to_n({1,0}),
	6 = rc_to_n({1,2}),
	9 = rc_to_n({2,2}),

	[1,9,8,5] = level1:doit({1,1}, level1:parse_input("input.txt")),

	ok.

