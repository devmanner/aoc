-module(solve2).
-export([test/0]).

%-compile(export_all).

parse(S) ->
    parse(S, 1).

parse([$d, $o, $n, $', $t, $(, $)|T], _Enabled) ->
    parse(T, 0);
parse([$d, $o, $(, $)|T], _Enabled) ->
    parse(T, 1);
parse([$m, $u, $l, $(|T], Enabled) ->
    case string:chr(T, $,) of
        0 -> parse(T, Enabled);
        N ->
            case catch {list_to_integer(string:substr(T, 1, N-1)),
                        list_to_integer(string:substr(T, N+1, string:chr(T, $)) - (N+1)))} of 
                {'EXIT', _WeKnowWhySoWeDontCare} ->
                    parse(T, Enabled);
                {X, Y} ->
                    Enabled*X*Y + parse(T, Enabled)
            end
    end;
parse([_|T], Enabled) ->
    parse(T, Enabled);
parse([], _Enabled) ->
    0.

test(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    parse(binary_to_list(Bin)).

test() ->
    48 = test("test2.txt"),
    63866497 = test("input.txt").
