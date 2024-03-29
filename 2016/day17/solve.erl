-module(solve).

-compile(export_all).

-define(PASSCODE, "njfxhljp").

is_open(D) when D =< 10 -> closed;
is_open(_D) -> open.

% This would in a C-like language be:
% Pred ? X : Y
if_else(true, X, _Y) -> X;
if_else(false, _X, Y) -> Y. 

door_states(Passcode, Path) ->
    <<U:4,D:4,L:4,R:4, _/binary>> = crypto:hash(md5, Passcode ++ Path),
    [is_open(U), is_open(D), is_open(L), is_open(R)].

door_states(Passcode, Row, Col, Path) ->
    [U, D, L, R] = door_states(Passcode, Path),
    [
        if_else(Row == 0, closed, U),
        if_else(Row == 3, closed, D),
        if_else(Col == 0, closed, L),
        if_else(Col == 3, closed, R)
    ].

shortest_path(Passcode) ->
    path(fun(X, Y) -> X < Y end, Passcode, 0, 0, []).

longest_path(Passcode) ->
    path(fun(X, Y) -> X > Y end, Passcode, 0, 0, []).

path(_Op, _Passcode, 3, 3, Path) ->
    Path;
path(Op, Passcode, Row, Col, Path) ->
    [Up, Down, Left, Right] = door_states(Passcode, Row, Col, Path),
    case opest_in_list(Op, [
                enter_door(Op, Passcode, Right, Row, Col+1, Path ++ [$R]),  % Right
                enter_door(Op, Passcode, Down, Row+1, Col, Path ++ [$D]),   % Down
                enter_door(Op, Passcode, Left, Row, Col-1, Path ++ [$L]),   % Left
                enter_door(Op, Passcode, Up, Row-1, Col, Path ++ [$U])      % Up
            ]) of
        [] -> dead_end;
        L -> L
    end.

enter_door(_Op, _Passcode, closed, _Row, _Col, _Path) ->
    dead_end;
enter_door(Op, Passcode, open, Row, Col, Path) ->
    path(Op, Passcode, Row, Col, Path).

opest_in_list(Op, L) ->
    case lists:filter(fun(X) -> X /= dead_end end, L) of
        [] -> [];
        [H|T] -> opest_in_list(Op, T, H)
    end.

opest_in_list(_Op, [], Acc) ->
    Acc;
opest_in_list(Op, [L|T], Acc) ->
    case Op(length(L), length(Acc)) of
        true -> opest_in_list(Op, T, L);
        false -> opest_in_list(Op, T, Acc)
    end.

do1() ->
    {MicroSeconds, RV} = timer:tc(fun() -> shortest_path(?PASSCODE) end),
    io:format("do1 took: ~pms~n", [MicroSeconds/1000]),
    RV.

do2() ->
    {MicroSeconds, RV} = timer:tc(fun() -> longest_path(?PASSCODE) end),
    io:format("do2 took: ~pms~n", [MicroSeconds/1000]),
    length(RV).

test() ->
    closed = is_open(1),
    closed = is_open(10), % 0xa
    open = is_open(11), % 0xb

    [open, open, open, closed] = door_states("hijkl", []),
    [open, closed, open, open] = door_states("hijkl", [$D]),
    [closed, closed, closed, closed] = door_states("hijkl", [$D, $R]),
    [closed, closed, closed, open] = door_states("hijkl", [$D, $U]),

    % "3" + empty path is a passphrase that implies 
    [closed, open, open, open] = door_states("3", 0, 2, []),
    [open, closed, open, open] = door_states("3", 3, 2, []),
    [open, open, closed, open] = door_states("3", 2, 0, []),
    [open, open, open, closed] = door_states("3", 2, 3, []),

    Shortest = fun(X, Y) -> X < Y end,
    [] = opest_in_list(Shortest, [dead_end, dead_end]),
    [1] = opest_in_list(Shortest, [[1], [1,2], dead_end]),
    [1] = opest_in_list(Shortest, [[1,2], [1], dead_end]),
    [1] = opest_in_list(Shortest, [dead_end, [1], [1,2]]),
    
    % Test cases given in problem description part 1
    "DDRRRD" = shortest_path("ihgpwlah"),
    "DDUDRLRRUDRD" = shortest_path("kglvqrro"),
    "DRURDRUDDLLDLUURRDULRLDUUDDDRR" = shortest_path("ulqzkmiv"),

    % Part 1
    "DURLDRRDRD" = do1(),

    % Test cases given in problem description part 2
    370 = length(longest_path("ihgpwlah")),
    492 = length(longest_path("kglvqrro")),
    830 = length(longest_path("ulqzkmiv")),

    % Part 2
    650 = do2(),

    ok.

