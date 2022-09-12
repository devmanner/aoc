-module(solve2).
-export([test/0]).

-export([next/1]).
-export([ones/1]).

% Length in bits
ones(L) ->
    {ToAlloc, Rem} = case L =< 8 of
        true -> {1, 8-L};
        false ->
            Alloc = (L div 8) + 1,
            {Alloc, Alloc*8 - L}
    end,
    io:format("Alloc: ~p~nRem: ~p~n", [ToAlloc, Rem]),
    <<R:L/bits, _:Rem/bits>> = binary:list_to_bin(lists:map(fun(_) -> 255 end, lists:seq(1, ToAlloc))),
    R.

next(B) ->
    Tail = crypto:exor(B, ones(erlang:bit_size(B))),
    <<B, 0:1, Tail>>.

test() ->
    <<4:3>> = next(<<1:1>>).
