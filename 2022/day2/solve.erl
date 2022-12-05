-module(solve).
-compile(nowarn_export_all).
-compile(export_all).

tr(1, $A) -> rock;
tr(1, $B) -> paper;
tr(1, $C) -> scissor;
tr(1, $X) -> rock;
tr(1, $Y) -> paper;
tr(1, $Z) -> scissor;

tr(2, $A) -> rock;
tr(2, $B) -> paper;
tr(2, $C) -> scissor;
tr(2, $X) -> loss;
tr(2, $Y) -> draw;
tr(2, $Z) -> win.

parse_file(Fname, Level) ->
    {ok, FD} = file:open(Fname, [read]),
    parse_file(FD, Level, []).
parse_file(FD, Level, Acc) ->
    case io:fread(FD, "", "~c ~c") of
        {ok, L} ->
            [A, B] = lists:map(fun([X]) -> tr(Level, X) end, L),
            parse_file(FD, Level, [{A, opp(A, B)}|Acc]);
        eof ->
            lists:reverse(Acc)
    end.

opp(X, draw) -> X;
opp(rock, win) -> paper;
opp(rock, loss) -> scissor;
opp(paper, win) -> scissor;
opp(paper, loss) -> rock;
opp(scissor, win) -> rock;
opp(scissor, loss) -> paper;
opp(_, X) -> X.

score(rock) -> 1;
score(paper) -> 2;
score(scissor) -> 3.

play(X, X) -> draw;
play(rock, paper) -> loss;
play(rock, scissor) -> win;
play(paper, rock) -> win;
play(paper, scissor) -> loss;
play(scissor, paper) -> win;
play(scissor, rock) -> loss.

play(L) ->
    play(L, 0, 0).
play([], APoint, BPoint) ->
    {APoint, BPoint};
play([{APlay, BPlay}|T], APoint, BPoint) ->
    case play(APlay, BPlay) of
        draw -> play(T, APoint+3+score(APlay), BPoint+3+score(BPlay));
        win -> play(T, APoint+6+score(APlay), BPoint+score(BPlay));
        loss -> play(T, APoint+score(APlay), BPoint+6+score(BPlay))
    end.

do1() ->
    {_APoint, BPoint} = play(parse_file("input.txt", 1)),
    BPoint.

do2() ->
    {_APoint, BPoint} = play(parse_file("input.txt", 2)),
    BPoint.

test() ->
    {_, 8} = play([{rock, paper}]),
    {_, 1} = play([{paper, rock}]),
    {_, 6} = play([{scissor, scissor}]),

    {15, _} = play(parse_file("test_input.txt", 1)),

    false = (11454 == do1()),

    % Level 2
    {15, _} = play(parse_file("test_input.txt", 2)),



    ok.
