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
	xsearch(ysearch(YMin, YMax), XMin, XMax).

ysimulate(YSpeed, YMin, YMax) ->
	ysimulate(0, 0, 1, YSpeed, YMin, YMax).
ysimulate(YPos, MaxYPos, Step, YSpeed, YMin, YMax) ->
	NextYPos = YPos + YSpeed,
	case NextYPos > YMax of
		true -> ysimulate(NextYPos, max(MaxYPos, NextYPos), Step+1, YSpeed-1, YMin, YMax);
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

xsearch([], _, _) ->
	[];
xsearch([{YSpeed, YMax, Steps}|T], XMin, XMax) ->
	io:format("xsearch: YSpeed: ~p Steps: ~p ~n", [YSpeed, Steps]),
	case bin_search_speed(?XSPDMIN, ?XSPDMAX, XMin, XMax, Steps) of
		miss ->
			xsearch(T, XMin, XMax);
		XSpeed ->
			io:format("Found Speed: ~p ~p~n", [XSpeed, YSpeed]),
			[{YMax, {XSpeed, YSpeed}}|xsearch(T, XMin, XMax)]
	end.

bin_search_speed(XSpeedMin, XSpeedMax, XMin, XMax, Steps) ->
	TestSpeed = (XSpeedMax + XSpeedMin) div 2,
	io:format("Simulating XSpeed: ~p XMin: ~p XMax: ~p Steps: ~p ", [TestSpeed, XMin, XMax, Steps]),
	R = xsimulate(TestSpeed, XMin, XMax, Steps),
	io:format("~p!~n", [R]),
	case xsimulate(TestSpeed, XMin, XMax, Steps) of
		hit -> TestSpeed;
		_ when XSpeedMin == XSpeedMax -> miss;
		short -> bin_search_speed(TestSpeed+1, XSpeedMax, XMin, XMax, Steps);
		long -> bin_search_speed(XSpeedMin, TestSpeed-1, XMin, XMax, Steps)
	end.

xsimulate(XSpeed, XMin, XMax, Steps) ->
    xsimulate(0, XSpeed, XMin, XMax, Steps).
xsimulate(XPos, XSpeed, XMin, XMax, Steps) when XSpeed > 0 ->
    xsimulate(XPos, XSpeed, -1, XMin, XMax, Steps);
xsimulate(XPos, XSpeed, XMin, XMax, Steps) when XSpeed < 0 ->
    xsimulate(XPos, XSpeed, 1, XMin, XMax, Steps);
xsimulate(XPos, XSpeed, XMin, XMax, Steps) when (Steps == 0) or (XSpeed == 0) ->
    case in_range(XPos, XMin, XMax) of
        true -> hit;
        false ->
            case XPos > XMax of
                true -> long;
                false -> short
            end
    end.
xsimulate(XPos, XSpeed, XDelta, XMin, XMax, Steps) ->
    NextXPos = XPos + XSpeed,
	xsimulate(NextXPos, XSpeed + XDelta, XMin, XMax, Steps-1).

in_range(A, AMin, AMax) ->
    (A >= AMin) and (A =< AMax).

