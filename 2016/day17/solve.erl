-module(solve).

-compile(export_all).

-define(PASSCODE, "njfxhljp").

is_open(C) ->
    case string:to_integer([C]) of
        {error, no_integer} ->
            case C of
                $a -> closed;
                _ -> open
            end;
        {X, []} when is_integer(X) -> closed
    end.

hash(Path) ->
    hash(?PASSCODE, Path).
hash(Passcode, Path) ->
    <<X1, X2, _/binary>> = crypto:hash(md5, Passcode ++ Path),
    string:lowercase(binary:bin_to_list(binary:encode_hex(<<X1,X2>>))).

door_state(Passcode, Row, Col, Path) ->
    [U, D, L, R] = lists:map(fun(X) -> is_open(X) end, hash(Passcode, Path)),
    Up = case Row == 0 of
        true -> closed;
        false -> U
    end,
    Down = case Row == 3 of
        true -> closed;
        false -> D
    end,
    Left = case Col == 0 of
        true -> closed;
        false -> L
    end,
    Right = case Col == 3 of
        true -> closed;
        false -> R
    end,
    [Up, Down, Left, Right].

shortest_path(Passcode) ->
    path(fun(X, Y) -> X < Y end, Passcode, 0, 0, []).

longest_path(Passcode) ->
    path(fun(X, Y) -> X > Y end, Passcode, 0, 0, []).

path(_Op, _Passcode, 3, 3, Path) ->
    Path;
path(Op, Passcode, Row, Col, Path) ->
    case length(Path) > 999 of
        true -> dead_end;
        false -> 
            [Up, Down, Left, Right] = door_state(Passcode, Row, Col, Path),
            case list(Op, [
                        path(Op, Passcode, Right, Row, Col+1, Path ++ [$R]),
                        path(Op, Passcode, Down, Row+1, Col, Path ++ [$D]),
                        path(Op, Passcode, Left, Row, Col-1, Path ++ [$L]),
                        path(Op, Passcode, Up, Row-1, Col, Path ++ [$U])
                    ]) of
                [] -> dead_end;
                L -> L
            end
    end.

path(_Op, _Passcode, closed, _Row, _Col, _Path) ->
    dead_end;
path(Op, Passcode, open, Row, Col, Path) ->
    path(Op, Passcode, Row, Col, Path).

list(Op, L) ->
    case lists:filter(fun(X) -> X /= dead_end end, L) of
        [] -> [];
        [H|T] -> list(Op, T, H)
    end.

list(_Op, [], Shortest) ->
    Shortest;
list(Op, [L|T], Shortest) ->
    case Op(length(L), length(Shortest)) of
        true -> list(Op, T, L);
        false -> list(Op, T, Shortest)
    end.

do1() ->
    shortest_path(?PASSCODE).

do2() ->
    length(longest_path(?PASSCODE)).

test() ->
    closed = is_open($1),
    open = is_open($A),

    "ced9" = hash("hijkl", []),
    "f2bc" = hash("hijkl", [$D]),
    "5745" = hash("hijkl", [$D, $R]),
    "528e" = hash("hijkl", [$D, $U]),

    Shortest = fun(X, Y) -> X < Y end,
    [] = list(Shortest, [dead_end, dead_end]),
    [1] = list(Shortest, [[1], [1,2], dead_end]),
    [1] = list(Shortest, [[1,2], [1], dead_end]),
    [1] = list(Shortest, [dead_end, [1], [1,2]]),
    
    "DDRRRD" = shortest_path("ihgpwlah"),
    "DDUDRLRRUDRD" = shortest_path("kglvqrro"),
    "DRURDRUDDLLDLUURRDULRLDUUDDDRR" = shortest_path("ulqzkmiv"),

    % Part 1
    "DURLDRRDRD" = do1(),

    % Part 2
    370 = length(longest_path("ihgpwlah")),
    492 = length(longest_path("kglvqrro")),
    830 = length(longest_path("ulqzkmiv")),

    650 = do2(),
    
    ok.

