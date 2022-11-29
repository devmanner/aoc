-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

dbg(Format, Arg) ->
    dbg(Format, Arg, false).
dbg(Format, Arg, true) ->
    io:format(Format, Arg);
dbg(_, _, false) ->
    ok.

value(X, _Reg) when is_integer(X) ->
    X;
value(X, Reg) when is_atom(X) ->
    maps:get(X, Reg).

reg_init() ->
    maps:from_list([{a, 0}, {b, 0}, {c, 0}, {d, 0}]).

%% Do nothing if outside the list
tgl_element(List, Element) when Element > length(List) ->
    List;
tgl_element(List, Element) when Element < 1 ->
    List;
tgl_element(List, Element) ->
    Value = tgl(lists:nth(Element, List)),
    lists:sublist(List, Element-1) ++ [Value] ++ lists:nthtail(Element,List).

tgl({inc, X}) ->
    {dec, X};
tgl({_, X}) ->
    {inc, X};
tgl({jnz, X, Y}) ->
    {cpy, X, Y};
tgl({_, X, Y}) ->
    {jnz, X, Y}.


find_a(Instr, Max) ->
    io:format("Spawning processes...~n"),
    find_a(Instr, 0, Max, []).

find_a(_Instr, Max, Max, Pids) ->
    Secs = 3,
    io:format("Letting processes run for ~ps...~n", [Secs]),
    timer:sleep(Secs*1000),
    io:format("Killing processes...~n"),
    lists:map(fun({_, Pid}) -> exit(Pid, killed_by_parent) end, Pids),
    io:format("Collecting results...~n"),
    collect_answers(Pids, []);
find_a(Instr, N, Max, Pids) ->
    Reg = maps:update(a, N, solve:reg_init()),
    {Pid, _MonRef} = spawn_monitor(fun() -> instr(Reg, [], Instr, undef) end),
    find_a(Instr, N+1, Max, [{N, Pid}|Pids]).

collect_answers([], []) ->
    non_toggling_found;
collect_answers([], R) ->
    R;
collect_answers([{N, Pid}|Pids], R) ->
    receive
        {'DOWN',_MonRef,process,Pid,non_toggling} ->
            collect_answers(Pids, R);
        {'DOWN',_MonRef,process,Pid,killed_by_parent} ->
%            io:format("Pid: ~p with a=~p was killed by parent. It's likely to be running infinitely~n", [Pid, N]),
            collect_answers(Pids, [N|R])
    end.

%instr(Reg, [], Instr) ->
%    {Pid, MonRef} = spawn_monitor(fun() -> instr(Reg, [], Instr, undef) end),
%    receive
%        {'DOWN',MonRef,process,Pid,non_toggling} ->
%            non_toggling
%    after
%        1000 ->
%            toggling
%    end.

instr(Reg, _, [], _) ->
    Reg;
instr(Reg, Done, [I={tgl, Off}|T], PrevOut) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    case value(Off, Reg) of
        Offset when Offset < 0 ->
            instr(Reg, tgl_element(Done, length(Done) + Offset + 1) ++ [I], T, PrevOut);
        0 ->
            instr(Reg, Done ++ [{inc, a}], T, PrevOut);
        Offset when Offset > 0 ->
            instr(Reg, Done ++ [I], tgl_element(T, Offset), PrevOut)
    end;
instr(Reg, Done, [I={cpy, Src, Dst}|T], PrevOut) when is_atom(Dst) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    instr(maps:update(Dst, value(Src, Reg), Reg), Done ++ [{cpy, Src, Dst}], T, PrevOut);
instr(Reg, Done, [I={inc, X}|T], PrevOut) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    instr(maps:update(X, maps:get(X, Reg) + 1, Reg), Done ++ [{inc, X}], T, PrevOut);
instr(Reg, Done, [I={dec, X}|T], PrevOut) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    instr(maps:update(X, maps:get(X, Reg) - 1, Reg), Done ++ [{dec, X}], T, PrevOut);
instr(Reg, Done, [I={jnz, X, Y}|T], PrevOut) ->
    dbg("Exec: ~p ~p~n", [I, Reg]),
    case value(X, Reg) =/= 0 of
        true ->
            {Done2, Todo} = lists:split(length(Done) + value(Y, Reg), Done ++ [{jnz, X, Y}|T]),
            instr(Reg, Done2, Todo, PrevOut);
        false ->
            instr(Reg, Done ++ [{jnz, X, Y}], T, PrevOut)
    end;
instr(Reg, Done, [I={out, X}|T], PrevOut) ->
    case value(X, Reg) of
        PrevOut ->
            exit(non_toggling);
        _ ->
            instr(Reg, Done ++ [I], T, value(X, Reg))
    end.

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
    I = parse_file("input.txt"),
    hd(find_a(I, 10000)).

test() ->
    I1 = [
            {cpy, 41, a},
            {inc, a},
            {inc, a},
            {dec, a},
            {jnz, a, 2},
            {dec, a}
        ],
    42 = maps:get(a, instr(reg_init(), [], I1, undef)),

    I2 = [
            {cpy, 1, a},
            {cpy, 1, b},
            {inc, b},
            {cpy, b, a}
        ],
    2 = maps:get(a, instr(reg_init(), [], I2, undef)),

    I3 = [
            {cpy, 10, a},
            {inc, b},
            {dec, a},
            {jnz, a, -2}
        ],
    10 = maps:get(b, instr(reg_init(), [], I3, undef)),


    I4 = [
            {cpy, 2, a},
            {tgl, a},
            {tgl, a},
            {tgl, a},
            {cpy, 1, a},
            {dec, a},
            {dec, a} 
        ],
    
    3 = maps:get(a, instr(reg_init(), [], I4, undef)),

    192 = do1(),

    ok.


