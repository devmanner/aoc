-module(solve).
-compile(export_all).

dbg(Format, Arg) ->
    dbg(Format, Arg, false).
dbg(Format, Arg, true) ->
    io:format(Format, Arg);
dbg(_, _, false) ->
    ok.

get_value(X, _) when is_integer(X) ->
    X;
get_value(X, Reg) when is_atom(X) ->
    maps:get(X, Reg).

reg_init() ->
    maps:from_list([{a, 0}, {b, 0}, {c, 0}, {d, 0}]).

instr(Reg, _, []) ->
    Reg;
instr(Reg, Done, [I={cpy, Src, Dst}|T]) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    instr(maps:update(Dst, get_value(Src, Reg), Reg), Done ++ [{cpy, Src, Dst}], T);
instr(Reg, Done, [I={inc, X}|T]) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    instr(maps:update(X, maps:get(X, Reg) + 1, Reg), Done ++ [{inc, X}], T);
instr(Reg, Done, [I={dec, X}|T]) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    instr(maps:update(X, maps:get(X, Reg) - 1, Reg), Done ++ [{dec, X}], T);
instr(Reg, Done, [I={jnz, X, Y}|T]) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    case value(Reg, X) =/= 0 of
        true ->
            {Done2, Todo} = lists:split(length(Done) + value(Reg, Y), Done ++ [{jnz, X, Y}|T]),
            instr(Reg, Done2, Todo);
        false ->
            instr(Reg, Done ++ [{jnz, X, Y}], T)
    end.

value(_Reg, X) when is_integer(X) ->
    X;
value(Reg, X) when is_atom(X) ->
    maps:get(X, Reg).

list_to_int_or_atom(X) ->
    case re:run(X, "[0-9]+") of
        nomatch -> list_to_atom(X);
        _ -> list_to_integer(X)
    end.

parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, []).
parse_file(FD, Acc) ->
    case file:read_line(FD) of
        {ok, Line} ->
            Tokens = string:split(string:strip(Line, right, $\n), " ", all),
            L = lists:map(fun(X) -> list_to_int_or_atom(X) end, Tokens),
            parse_file(FD, [list_to_tuple(L)|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

do1() ->
    I = parse_file("input1.txt"),
    Reg = instr(reg_init(), [], I),
    maps:get(a, Reg).

do2() ->
    I = parse_file("input1.txt"),
    InitReg = maps:update(c, 1, reg_init()),
    Reg = instr(InitReg, [], I),
    maps:get(a, Reg).


test() ->
    I1 = [
            {cpy, 41, a},
            {inc, a},
            {inc, a},
            {dec, a},
            {jnz, a, 2},
            {dec, a}
        ],
    42 = maps:get(a, instr(reg_init(), [], I1)),

    I2 = [
            {cpy, 1, a},
            {cpy, 1, b},
            {inc, b},
            {cpy, b, a}
        ],
    2 = maps:get(a, instr(reg_init(), [], I2)),

    I3 = [
            {cpy, 10, a},
            {inc, b},
            {dec, a},
            {jnz, a, -2}
        ],
    10 = maps:get(b, instr(reg_init(), [], I3)),

    ok.


