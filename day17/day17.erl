-module(day17).

-define(XSPDMIN, -100).
-define(XSPDMAX, 100).

-compile(export_all).

parse_input(Fname) ->
	{ok, FD} = file:open(Fname, [read]),
	{ok, [X1, X2, Y1, Y2]} = io:fread(FD, "", "target area: x=~d..~d, y=~d..~d"),
	{min(X1, X2), max(X1, X2), min(Y1, Y2), max(Y1, Y2)}.

doit(Fname) ->
	{XMin, XMax, YMin, YMax} = parse_input(Fname),
	PossibleX = xsearch(min(XMax, 0), max(XMax, 0), XMin, XMax),
	ysearch(PossibleX, YMin, YMax).

ysimulate(YSpeed, YMin, YMax) ->
	ysimulate(0, YSpeed, YMin, YMax, 1).
ysimulate(YPos, YSpeed, YMin, YMax, Step) ->
	NextYPos = YPos + YSpeed,
	case in_range(NextYPos, YMin, YMax) of
		true -> MaxYPos;
		false ->
			case NextYPos < YMin of
				true -> miss;
				false > 
					ysimulate(NextYPos, max(MaxYPos, NextYPos), Step+1, YSpeed-1, YMin, YMax);
		false -> 
			case NextYPos < YMin of
				true -> miss;
				false -> {MaxYPos, Step}
			end
	end.

ysearch(YMin, YMax) ->
	ysearch(0, YMin, YMax).
ysearch(YSpeed, YMin, YMax) ->
	case ysimulate(YSpeed, YMin, YMax) of
		miss -> [];
		{MaxYPos, Step} -> [{YSpeed, MaxYPos, Step}|ysearch(YSpeed+1, YMin, YMax)]
	end.

xsearch(XMinSpeed, XMaxSpeed, XMin, XMax) ->
	F = fun(Speed, Acc) ->
		case xsimulate(Speed, XMin, XMax) of
			miss -> Acc;
			Steps ->
				[{Speed, Steps}|Acc]
		end
	end,
	io:format("Searching from: ~p to ~p~n", [XMinSpeed, XMaxSpeed]), 
	lists:foldl(F, [], lists:seq(XMinSpeed, XMaxSpeed)).

xsimulate(XSpeed, XMin, XMax) when XMin < YMin ->
    xsimulate(0, XSpeed, XMin, XMax, 1).
xsimulate(XPos, XSpeed, XMin, XMax, Steps) when XSpeed > 0 ->
    xsimulate(XPos, XSpeed, -1, XMin, XMax, Steps);
xsimulate(XPos, XSpeed, XMin, XMax, Steps) when XSpeed < 0 ->
    xsimulate(XPos, XSpeed, 1, XMin, XMax, Steps);
xsimulate(XPos, XSpeed, XMin, XMax, Steps) when XSpeed == 0 ->
    case in_range(XPos, XMin, XMax) of
		true -> Steps;
		false -> miss
	end.
xsimulate(XPos, XSpeed, XDelta, XMin, XMax, Steps) ->
	NextXPos = XPos + XSpeed,
    case in_range(NextXPos, XMin, XMax) of
        true -> Steps;
        false ->
            case XPos > XMax of
                true -> miss;
                false -> 
					xsimulate(XPos+XSpeed, XSpeed+XDelta, XMin, XMax, Steps+1)
            end
    end.

in_range(A, AMin, AMax) ->
    (A >= AMin) and (A =< AMax).

