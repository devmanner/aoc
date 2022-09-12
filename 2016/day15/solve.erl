-module(solve).
-compile(export_all).

%% Disc #1 has 7 positions; at time=0, it is at position 0.
%% Disc #2 has 13 positions; at time=0, it is at position 0.
%% Disc #3 has 3 positions; at time=0, it is at position 2.
%% Disc #4 has 5 positions; at time=0, it is at position 2.
%% Disc #5 has 17 positions; at time=0, it is at position 0.
%% Disc #6 has 19 positions; at time=0, it is at position 7.

do1() ->
    % Answer is the state at t0 that the disks should have in order for a fall-through.
    Answer = [6, 11, 0, 1, 12, 13],
    do(0, 999999, calc1, Answer).

do2() ->
    Answer = [6, 11, 0, 1, 12, 13, 4],
    do(0, 9999999, calc2, Answer).

do(XMax, XMax, _F, _Answer) ->
    not_found;
do(X, XMax, F, Answer) ->
    case apply(?MODULE, F, [X]) of
        Answer -> 
            X;
        _ ->
            do(X+1, XMax, F, Answer)
    end.

calc1(X) ->
    [
        % X + start_pos rem n_positions
        (X + 0) rem 7,
        (X + 0) rem 13,
        (X + 2) rem 3,
        (X + 2) rem 5,
        (X + 0) rem 17,
        (X + 7) rem 19
    ].

calc2(X) ->
    [
        (X + 0) rem 7,
        (X + 0) rem 13,
        (X + 2) rem 3,
        (X + 2) rem 5,
        (X + 0) rem 17,
        (X + 7) rem 19,
        (X + 0) rem 11
    ].

test() ->
    121834 = do1(),
    3208099 = do2(),
    ok.
