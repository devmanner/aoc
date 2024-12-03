-module(solve1).

-compile(export_all).

parse([$m, $u, $l, $(|T]) ->
    case string:chr(T, $,) of
        0 -> parse(T);
        N ->
            case catch {list_to_integer(string:substr(T, 1, N-1)),
                        list_to_integer(string:substr(T, N+1, string:chr(T, $)) - (N+1)))} of 
                {'EXIT', _WeKnowWhySoWeDontCare} ->
                    parse(T);
                {X, Y} ->
                    X*Y + parse(T)
            end
    end;
parse([_|T]) ->
    parse(T);
parse([]) ->
    0.

test(Fname) ->
    {ok, Bin} = file:read_file(Fname),
    parse(binary_to_list(Bin)).

test() ->
    161 = test("test1.txt"),
    171183089 = test("input.txt").
