-module(day14).
-export([parse_input/1, t/2, calc/2, test/0]).

remove_dups([]) ->
    [];
remove_dups([H|T]) ->
    [H | [X || X <- remove_dups(T), X /= H]].

parse_input(Fname) ->
	{ok, FP} = file:open(Fname, [read]),
	{ok, Template} = file:read_line(FP),
	{ok, "\n"} = file:read_line(FP),
	{string:strip(Template, right, $\n), parse_input(FP, dict:new())}.
parse_input(FP, Rules) ->
	case file:read_line(FP) of
		{ok, [L,R,$\ ,$-,$>,$\ ,S,$\n]} ->
			parse_input(FP, dict:append([L,R], S, Rules));
		eof ->
			Rules
	end.

t([X], _) -> [X];
t([L,R|T], Rules) ->
	case dict:find([L,R], Rules) of
		{ok, [New]} -> [L,New|t([R|T], Rules)];
		error -> [L|t([R|T], Rules)]
	end.

calc(Fname, N) ->
	{Template, Rules} = parse_input(Fname),
	S = lists:foldl(fun(X, Acc) -> t(Acc, Rules) end, Template, lists:seq(1, N)),
	Cnt = lists:map(fun(C) -> length(lists:filter(fun(X) -> X==C end, S)) end, remove_dups(S)),
	{lists:max(Cnt) - lists:min(Cnt), S}.

test() ->
	{_, "NCNBCHB"} = calc("practice.input", 1),
    {_, "NBCCNBBBCBHCB"} = calc("practice.input", 2),
    {_, "NBBBCNCCNBBNBNBBCHBHHBCHB"} = calc("practice.input", 3),
    {_, "NBBNBNBBCCNBCNCCNBBNBBNBBBNBBNBBCBHCBHHNHCBBCBHCB"} = calc("practice.input", 4),
	{1588, _} = calc("practice.input", 10),
	ok.

