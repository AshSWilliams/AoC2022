-module(day_8).

-compile(export_all).

problem(1) ->
    {ok, Input} = file:open("input.txt", read),
    InputList = lists:reverse(read_to_list(Input)),
    TreeGrid = lists:map(fun(A) -> [[X] || X <- A] end, InputList),
    count_visible(TreeGrid, TreeGrid, 1, 1, 0);
problem(2) ->
    {ok, Input} = file:open("input.txt", read),
    InputList = lists:reverse(read_to_list(Input)),
    TreeGrid = lists:map(fun(A) -> [[X] || X <- A] end, InputList),
    max_score(TreeGrid, TreeGrid, 1, 1, 0).

max_score([], _TreeGrid, _X, _Y, CurrentScore) ->
    CurrentScore;
max_score([[] | Rows] = _It, TreeGrid, X, _Y, CurrentScore) ->
    max_score(Rows, TreeGrid, X + 1, 1, CurrentScore);
max_score([[Tree | RowTrees] | Rows], TreeGrid, X, Y, CurrentScore) ->
    %io:format("Calculating score for tree of height ~p in row ~p and column ~p~n", [Tree, X, Y]),
    %io:format("Tree grid is ~p~n", [TreeGrid]),
    Score = get_score(Tree, TreeGrid, X, Y, no),
    %io:format("Score calculated as ~p~n", [Score]),
    case Score > CurrentScore of
        true ->
            %io:format("New highest score of ~p for tree of height ~p in row ~p and column ~p~n", [Score, Tree, X, Y]),
            get_score(Tree, TreeGrid, X, Y, log),
            max_score([RowTrees | Rows], TreeGrid, X, Y + 1, Score);
        _ ->
            max_score([RowTrees | Rows], TreeGrid, X, Y + 1, CurrentScore)
    end.

get_score(_TreeHeight, _TreeGrid, 1, _Y, _) ->
    0;
get_score(_TreeHeight, _TreeGrid, 99, _Y, _) ->
    0;
get_score(_TreeHeight, _TreeGrid, _X, 1, _) ->
    0;
get_score(_TreeHeight, _TreeGrid, _X, 99, _) ->
    0;
get_score(TreeHeight, TreeGrid, X, Y, Log) ->
    CanSeeToNorth = view_distance(TreeHeight, TreeGrid, X, Y, north, Log),
    CanSeeToSouth = view_distance(TreeHeight, TreeGrid, X, Y, south, Log),
    CanSeeToWest = view_distance(TreeHeight, TreeGrid, X, Y, west, Log),
    CanSeeToEast = view_distance(TreeHeight, TreeGrid, X, Y, east, Log),
    case Log of 
        log ->
            io:format("Can see ~p to north~n", [CanSeeToNorth]),
            io:format("Can see ~p to south~n", [CanSeeToSouth]),
            io:format("Can see ~p to west~n", [CanSeeToWest]),
            io:format("Can see ~p to east~n", [CanSeeToEast]);
        _ -> ok
    end,
    CanSeeToNorth * CanSeeToSouth * CanSeeToWest * CanSeeToEast.


view_distance(TreeHeight, Grid, X, Y, north, Log) ->
    ColumnHeights = [lists:nth(Y, Row) || Row <- Grid],
    ToNorth = lists:reverse(lists:sublist(ColumnHeights, 1, X - 1)),
    case Log of
        log ->
            io:format("Trees to the north: ~p~n", [ToNorth]);
        _ -> ok
    end,
    calculate_view_distance(TreeHeight, ToNorth);
view_distance(TreeHeight, Grid, X, Y, south, Log) ->
    ColumnHeights = [lists:nth(Y, Row) || Row <- Grid],
    ToSouth = lists:sublist(ColumnHeights, X + 1, 99 - X + 1),
    case Log of
        log ->
            io:format("Trees to the south: ~p~n", [ToSouth]);
        _ -> ok
    end,
    calculate_view_distance(TreeHeight, ToSouth);
view_distance(TreeHeight, Grid, X, Y, west, Log) ->
    RowHeights = lists:nth(X, Grid),
    ToWest = lists:reverse(lists:sublist(RowHeights, 1, Y - 1)),
    case Log of
        log ->
            io:format("Trees to the west: ~p~n", [ToWest]);
        _ -> ok
    end,
    calculate_view_distance(TreeHeight, ToWest);
view_distance(TreeHeight, Grid, X, Y, east, Log) ->
    RowHeights = lists:nth(X, Grid),
    ToEast = lists:sublist(RowHeights, Y + 1, 99 - Y + 1),
    case Log of
        log ->
            io:format("Trees to the east: ~p~n", [ToEast]);
        _ -> ok
    end,
    calculate_view_distance(TreeHeight, ToEast).

calculate_view_distance(Height, Trees) ->
    calculate_view_distance(Height, Trees, 1).
calculate_view_distance(Height, [], N) ->
    N - 1;
calculate_view_distance(Height, [TreeHeight | Trees], N) ->
    case Height > TreeHeight of
        true ->
            calculate_view_distance(Height, Trees, N + 1);
        false ->
            N
    end.

count_visible([], _TreeGrid, _X, _Y, Visible) ->
    Visible;
count_visible([[] | Rows] = _It, TreeGrid, _X, Y, Visible) ->
    count_visible(Rows, TreeGrid, 1, Y + 1, Visible);
count_visible([[Tree | RowTrees] | Rows], TreeGrid, X, Y, Visible) ->
    case is_visible(Tree, TreeGrid, X, Y) of
        true ->
            %% io:format("Tree of height ~p in row ~p and column ~p is visible~n", [Tree, Y, X]),
            count_visible([RowTrees | Rows], TreeGrid, X + 1, Y, Visible + 1);
        _ ->
            %% io:format("Tree of height ~p in row ~p and column ~p is not visible~n", [Tree, Y, X]),
            count_visible([RowTrees | Rows], TreeGrid, X + 1, Y, Visible)
    end.

is_visible(_TreeHeight, _TreeGrid, 1, _Y) ->
    true;
is_visible(_TreeHeight, _TreeGrid, 99, _Y) ->
    true;
is_visible(_TreeHeight, _TreeGrid, _X, 1) ->
    true;
is_visible(_TreeHeight, _TreeGrid, _X, 99) ->
    true;
is_visible(TreeHeight, TreeGrid, X, Y) ->
    %% io:format("Checking if tree of height ~p in row ~p and column ~p is visible~n", [TreeHeight, X, Y]),
    RowHeights = lists:nth(Y, TreeGrid),
    ColumnHeights = [lists:nth(X, Row) || Row <- TreeGrid],
    ToNorth = lists:max(lists:sublist(ColumnHeights, 1, Y - 1)), %% Don't include the height of the tree we're inspecting
    ToSouth = lists:max(lists:sublist(ColumnHeights, Y + 1, 99 - Y + 1)),
    ToWest = lists:max(lists:sublist(RowHeights, 1, X - 1)),
    ToEast = lists:max(lists:sublist(RowHeights, X + 1, 99 - X + 1)),
    TreeHeight > ToNorth orelse TreeHeight > ToSouth orelse TreeHeight > ToWest orelse TreeHeight > ToEast.

read_to_list(Input) ->
    case file:read_line(Input) of
        {ok, Line} ->
            read_to_list(Input, [string:trim(Line, trailing, "\n")]);
        eof ->
            []
    end.
read_to_list(Input, []) ->
    read_to_list(Input);
read_to_list(Input, Read) ->
        case file:read_line(Input) of
        {ok, Line} ->
            read_to_list(Input, [string:trim(Line, trailing, "\n") | Read]);
        eof ->
            Read
    end.