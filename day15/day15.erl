-module(day15).
-export([parse_input/1, update/0, part2/0, mgetcost/2, mgetpath/2, mprintcost/0, mprintpath/0]).

-define(HIGH_COST, 9999999999999).

mset(Row, Col, Value) ->
	ets:insert(maze, {{Row, Col}, Value}).
mgetpath(Row, Col) ->
	case ets:lookup(maze, {Row, Col}) of
        [{{Row, Col}, {_Cost, Path}}] ->
			Path;
		[] ->
			?HIGH_COST
	end.
mgetcost(Row, Col) ->
    case ets:lookup(maze, {Row, Col}) of
        [{{Row, Col}, {Cost, _Path}}] ->
            Cost;
        [] ->
            ?HIGH_COST
    end.

mprint(Fun) ->
	lists:foreach(fun(Row) ->
		lists:foreach(fun(Col) ->
			io:format("~p ", [Fun(Row, Col)])
		end, lists:seq(0, getcols()-1)),
		io:format("~n")
	end, lists:seq(0, getrows()-1)).

mprintcost() ->
	mprint(fun(Row, Col) -> mgetcost(Row, Col) end).
mprintpath() ->
    mprint(fun(Row, Col) -> mgetpath(Row, Col) end).

getrows() ->
	[{rows, Rows}] = ets:lookup(maze, rows),
	Rows.
getcols() ->
    [{cols, Cols}] = ets:lookup(maze, cols),
    Cols.

parse_input(Fname) when is_list(Fname) ->
	{ok, FP} = file:open(Fname, [read]),
	maze = ets:new(maze, [set, named_table]),
	{Row, Col} = parse_input(FP, 0, 0, 0),
	C = mgetcost(0, 0),
	mset(0, 0, {C, 0}),
	ets:insert(maze, {rows, Row}),
    ets:insert(maze, {cols, Col}).	
parse_input(FP, Row, Col, MaxCol) ->
	case file:read(FP, 1) of
		{ok, "\n"} ->
			parse_input(FP, Row+1, 0, max(MaxCol, Col));
		{ok, C} ->
			mset(Row, Col, {list_to_integer(C), ?HIGH_COST}),
			parse_input(FP, Row, Col+1, max(MaxCol, Col));
		eof ->
			{Row, MaxCol}
	end.

part2() ->
	lists:foreach(fun(R) ->
		lists:foreach(fun(C) ->
    		lists:foreach(fun(Row) ->
        		lists:foreach(fun(Col) ->
            		mset(R*getrows() + Row, C*getcols() + Col, {((mgetcost(Row, Col) + R + C - 1) rem 9)+1, ?HIGH_COST})
        		end, lists:seq(0, getcols()-1))
	    	end, lists:seq(0, getrows()-1))
		end, lists:seq(0, 5))
	end, lists:seq(0, 5)),
    C = mgetcost(0, 0),
    mset(0, 0, {C, 0}),
	ets:insert(maze, {rows, getrows()*5}),        
    ets:insert(maze, {cols, getcols()*5}),
	{getrows(), getcols()}.


update(Row, Col, CompCost) ->
	Cost = mgetcost(Row, Col),
	PathCost = mgetpath(Row, Col),
    case Cost + CompCost < PathCost of
        true ->
			mset(Row, Col, {Cost, Cost + CompCost}),
			update;
        false ->
			no_update
    end.

update(Row, Col) ->
	PathCost = mgetpath(Row, Col),
	R = [
    	update(Row-1, Col, PathCost),
    	update(Row, Col-1, PathCost),
    	update(Row+1, Col, PathCost),
		update(Row, Col+1, PathCost)
	],
	case lists:filter(fun(update) -> true; (_) -> false end, R) of
		[] -> no_update;
		_ -> update
	end.

update() ->
	io:format("Update iternation~n"),
	R = lists:flatten(
			lists:map(fun(Row) ->
				lists:map(fun(Col) ->
					update(Row, Col)
			end, lists:seq(0, getcols()-1))
		end, lists:seq(0, getrows()-1))),
	case lists:filter(fun(update) -> true; (_) -> false end, R) of
        [] ->
			mprintpath(),
			mgetpath(getrows()-1, getcols()-1);
        _ -> update()
    end.

