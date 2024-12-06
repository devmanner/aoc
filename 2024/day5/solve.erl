-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

parse(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    Rules = parse_rules(FD, orddict:new()),
    Updates = parse_updates(FD),
    {Rules, Updates}.
parse_rules(FD, Rules) ->
    case file:read_line(FD) of
        {ok,"\n"} -> Rules;
        {ok, S} ->
            [F, T] = lists:map(fun(X) -> erlang:binary_to_integer(X) end, re:split(string:chomp(S), "\\|")),
            case orddict:find(F, Rules) of
                error -> parse_rules(FD, orddict:append(F, T, orddict:store(F, [], Rules)));
                _ -> parse_rules(FD, orddict:append(F, T, Rules))
            end
    end.
parse_updates(FD) ->
    case file:read_line(FD) of
        eof -> [];
        {ok, S} ->
            [lists:map(fun(X) -> erlang:binary_to_integer(X) end, re:split(string:chomp(S), ","))|parse_updates(FD)]
    end.

right_order(_Rules, []) ->
    true;
right_order(Rules, [H|T]) ->
    Fun = fun
        (_, false) -> false;
        (X, true) ->
            case orddict:find(X, Rules) of
                error -> true;
                {ok, L} -> not lists:member(H, L)
            end
        end,
    case lists:foldl(Fun, true, T) of
        true -> right_order(Rules, T);
        false -> false
    end.

filter_out_wrong_order(Rules, Updates) ->
    lists:filter(fun(U) -> right_order(Rules, U) end, Updates).

filter_out_right_order(Rules, Updates) ->
    lists:filter(fun(U) -> not right_order(Rules, U) end, Updates).

middle_element(L) ->
    {_, T} = lists:split(trunc(math:floor(length(L)/2)), L),
    hd(T).

sum_middle_elements(Lol) ->
    lists:foldl(fun(X, Acc) -> X+Acc end, 0, lists:map(fun(L) -> middle_element(L) end, Lol)).

fix_updates(Rules, Updates) ->
    lists:map(fun(Update) -> fix_update(Rules, Update) end, Updates).
fix_update(Rules, Update) ->
    %% Should X be sorted before Y?
    F = fun(X, Y) ->
        case orddict:find(X, Rules) of
            error -> false;
            {ok, L} ->
                lists:member(Y, L)
        end
    end,
    lists:sort(F, Update).

test() ->
    {R1, U1} = parse("test1.txt"),
    143 = sum_middle_elements(filter_out_wrong_order(R1, U1)),

    {R2, U2} = parse("input.txt"),
    6612 = sum_middle_elements(filter_out_wrong_order(R2, U2)),

    123 = sum_middle_elements(fix_updates(R1, filter_out_right_order(R1, U1))),
    sum_middle_elements(fix_updates(R2, filter_out_right_order(R2, U2))).

