-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

arr_swap(A, Idx1, Idx2) ->
    V1 = array:get(Idx1, A),
    V2 = array:get(Idx2, A),
    array:set(Idx2, V1, array:set(Idx1, V2, A)).

is_odd(X) ->
    (X band 1) == 1.

block(What, Len) ->
    lists:map(fun(_) -> What end, lists:seq(1, Len)).

create_disk(L) ->
    array:from_list(lists:flatten(create_disk(L, 0, file))).
create_disk([], _, _) ->
    [];
create_disk([H|T], Id, file) ->
    [block(Id, H)|create_disk(T, Id+1, free)];
create_disk([H|T], Id, free) ->
    [block(free, H)|create_disk(T, Id, file)].

print_disk(L) ->
    F = fun (_, free, _) ->
                io:format(".");
            (_, Id, _) ->
                io:format("~p", [Id])
        end, 
    array:foldl(F, ok, L),
    io:format("~n").

comp1(L) ->
    comp1(L, 0, array:size(L)-1).
comp1(L, Idx, Idx) ->
    L;
comp1(L, Idx1, Idx2) ->
    case {array:get(Idx1, L), array:get(Idx2, L)} of
        {free, free} ->
            comp1(L, Idx1, Idx2-1);
        {free, _} ->
            comp1(arr_swap(L, Idx1, Idx2), Idx1+1, Idx2-1);
        {_, free} ->
            comp1(L, Idx1+1, Idx2-1);
        {_, _} ->
            comp1(L, Idx1+1, Idx2)
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


find_prev_file(L, Idx) ->
    case array:get(Idx, L) of
        free -> find_prev_file(L, Idx-1);
        Id -> {find_beginning_of_file(L, Idx, Id), Idx}
    end.
find_beginning_of_file(_L, 0, _Id) ->
    0;
find_beginning_of_file(L, Idx, Id) ->
    case array:get(Idx-1, L) of
        Id -> find_beginning_of_file(L, Idx-1, Id);
        _ -> Idx
    end.

fits_file(_L, _Idx, 0) ->
    true;
fits_file(L, Idx, Len) ->
    case array:get(Idx, L) of
        free -> fits_file(L, Idx+1, Len-1);
        _ -> false
    end.

find_free_slot(_L, Idx, _Size, Max) when Idx > Max ->
    oob;
find_free_slot(L, Idx, Size, Max) ->
    case fits_file(L, Idx, Size) of
        true -> Idx;
        false -> find_free_slot(L, Idx+1, Size, Max)
    end.

comp2(L) ->
    comp2(L, array:size(L)-1).
comp2(L, Pos) when Pos < 1 ->
    L;
comp2(L, Pos) ->
    {S, E} = find_prev_file(L, Pos),
    Size = E-S+1,
    case find_free_slot(L, 0, Size, S) of
        oob ->
            comp2(L, S-1);
        FreeStart ->
            Fun = fun(I, Acc) ->
                arr_swap(Acc, I+S, I+FreeStart)
            end,
            NewL = lists:foldl(Fun, L, lists:seq(0, Size-1)),
            comp2(NewL, S-1)
    end.

checksum(L) ->
    Fn = fun(_, free, {Pos, Sum}) -> {Pos+1, Sum};
            (_, Id, {Pos, Sum}) -> {Pos+1, Sum + Id*Pos}
        end,
    {_, Checksum} = array:foldl(Fn, {0, 0}, L),
    Checksum.

parse_file(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    create_disk(lists:map(fun list_to_integer/1, re:split(Bin, "", [{return, list}, trim]))).

test() ->
    1928 = checksum(solve:comp1(solve:parse_file("test.txt"))),
    6200294120911 = checksum(solve:comp1(solve:parse_file("input.txt"))),

    2858 = checksum(solve:comp2(solve:parse_file("test.txt"))),
    6227018762750 = checksum(solve:comp2(solve:parse_file("input.txt"))).
