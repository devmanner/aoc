-module(solve).
-compile(export_all).

%% Disc #1 has 7 positions; at time=0, it is at position 0.
%% Disc #2 has 13 positions; at time=0, it is at position 0.
%% Disc #3 has 3 positions; at time=0, it is at position 2.
%% Disc #4 has 5 positions; at time=0, it is at position 2.
%% Disc #5 has 17 positions; at time=0, it is at position 0.
%% Disc #6 has 19 positions; at time=0, it is at position 7.

%% n + 0 rem 7 = 6
%% n + 0 rem 13 = 11
%% n + 2 rem 3 = 0
%% n + 2 rem 5 = 1
%% n + 0 rem 17 = 12
%% n + 7 rem 19 = 13

%% n = 6 + 7k1
%% n = 11 + 13k2
%% n = -2 + 3k3
%% n = -1 + 5k4
%% n = 12 + 17k5
%% n = 6 + 19k6

% Find k[1-6]

do1() ->
    % Answer is the state at t0 that the discs should have in order for a fall-through.
    Answer = [6, 11, 0, 1, 12, 13],
    do(6, 999999, 19, calc1, Answer).

do2() ->
    Answer = [6, 11, 0, 1, 12, 13, 4],
    do(6, 9999999, 19, calc2, Answer).

do(X, XMax, _Step, _F, _Answer) when X > XMax ->
    not_found;
do(X, XMax, Step, F, Answer) ->
    case apply(?MODULE, F, [X]) of
        Answer -> 
            X;
        _ ->
            do(X+Step, XMax, Step, F, Answer)
    end.

calc1(X) ->
    [
        % Calculate disc positions at time X
        % Position of each disc is:
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
    {Time, ok} = timer:tc(?MODULE, do_test, []),
    io:format("Test took: ~p ms~n", [Time/1000]).

do_test() ->
    121834 = do1(),
    3208099 = do2(),
    ok.
