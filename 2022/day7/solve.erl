-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

% {dir, "/", [{dir, "usr", [...]}, {file, "foo.txt", 12}]}
parse_file(Fname) ->
    {ok, FD} = file:open(Fname, [read]),
    Lines = read_file(FD, []),
    parse_lines(Lines, [], [{dir, "/", []}]).

read_file(FD, Acc) ->
    case file:read_line(FD) of
        {ok, Line} ->
            read_file(FD, [string:chomp(Line)|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

parse_lines([], _CWD, Tree) ->
    Tree;
parse_lines([[$$, $ , $c, $d, $ , $., $. |_]|T], CWD, Tree) ->
    {CWD2, _} = lists:split(length(CWD) - 1, CWD),
    parse_lines(T, CWD2, Tree);
parse_lines([[$$, $ , $c, $d, $  |Rest]|T], CWD, Tree) ->
    parse_lines(T, CWD ++ [Rest], Tree);
parse_lines([[$$, $ , $l, $s|_]|T], CWD, Tree) ->
    {L, Files} = parse_ls(T, []),
    parse_lines(L, CWD, add_files_to_tree(Tree, CWD, Files)).

add_files_to_tree([], [], Files) ->
    Files;
add_files_to_tree([{dir, Dir, Contens}|Tree], [Dir|T], Files) ->
    [{dir, Dir, add_files_to_tree(Contens, T, Files)}|Tree];
add_files_to_tree([NoMatch|Tree], Path, Files) ->
    [NoMatch|add_files_to_tree(Tree, Path, Files)].

parse_ls([], Acc) ->
    {[], Acc};
parse_ls([[$$|_]|_]=L, Acc) ->
    {L, Acc};
parse_ls([[$d, $i, $r, $ |Dirname]|T], Acc) ->
    parse_ls(T, [{dir, Dirname, []}|Acc]);
parse_ls([Filespec|T], Acc) ->
    {ok,[Size, Fname],[]} = io_lib:fread("~d ~s", Filespec),
    parse_ls(T, [{file, Size, Fname}|Acc]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

dir_size({file, Size, _Name}) ->
    Size;
dir_size({dir, _Name, Contens}) ->
    dir_size(Contens);
dir_size([]) ->
    0;
dir_size([X|T]) ->
    dir_size(X) + dir_size(T).

foldl(_Fun, Acc, []) ->
    Acc;
foldl(Fun, AccIn, [{dir, _Name, Contens}=X|T]) ->
    Acc2 = Fun(X, AccIn),
    foldl(Fun, foldl(Fun, Acc2, Contens), T);
foldl(Fun, AccIn, [X|T]) ->
    foldl(Fun, Fun(X, AccIn), T).

do1() ->
    F = fun ({file, _Size, _Name}, Acc) ->
                Acc;
            ({dir, Name, _Contens}=X, Acc) ->
                Size = dir_size(X),
                case Size < 100000 of
                    true -> [{Name, Size}|Acc];
                    false -> Acc
                end
    end,

    G = fun({_Name, Size}, Acc) ->
        Size + Acc
    end,

    Tree = parse_file("input.txt"),
    lists:foldl(G, 0, foldl(F, [], Tree)).

test() ->
    ok.




