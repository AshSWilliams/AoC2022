-module(day_4).

-compile(export_all).

problem(1) ->
    {ok, Input} = file:open("input.txt", read),
    count_overlaps(read_to_list(Input), 0);
problem(2) -> 
    {ok, Input} = file:open("input.txt", read),
    count_intersections(read_to_list(Input), 0).


count_overlaps([], Overlaps) ->
    Overlaps;
count_overlaps([Pair | Cons], Overlaps) ->
    [Section1, Section2] = string:tokens(Pair, ","),
    case has_overlap(Section1, Section2, contain) orelse has_overlap(Section2, Section1, contain) of
        true -> 
            %% io:format("~p and ~p overlap. Total so far: ~p~n", [Section1, Section2, Overlaps + 1]),
            count_overlaps(Cons, Overlaps + 1);
        false ->
            %% io:format("~p and ~p DO NOT overlap. Total so far: ~p~n", [Section1, Section2, Overlaps]),
            count_overlaps(Cons, Overlaps)
    end.

count_intersections([], Intersections) ->
    Intersections;
count_intersections([Pair | Cons], Intersections) ->
    [Section1, Section2] = string:tokens(Pair, ","),
    case has_overlap(Section1, Section2, intersect) orelse has_overlap(Section2, Section1, intersect) of
        true -> 
            %% io:format("~p and ~p overlap. Total so far: ~p~n", [Section1, Section2, Intersections + 1]),
            count_intersections(Cons, Intersections + 1);
        false ->
            %% io:format("~p and ~p DO NOT overlap. Total so far: ~p~n", [Section1, Section2, Intersections]),
            count_intersections(Cons, Intersections)
    end.


%% Is section 2 contained within section 1?
has_overlap(Section1, Section2, Type) ->
    [S1Start, S1End] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Section1, "-")),
    [S2Start, S2End] = lists:map(fun(X) -> list_to_integer(X) end, string:tokens(Section2, "-")),
    case Type of 
        contain ->
            S1Start =< S2Start andalso S1End >= S2End;
        intersect ->
            %% Is either edge of the second range within the first range?
            (S1Start =< S2Start andalso S2Start =< S1End) orelse (S1Start =< S2End andalso S2End =< S1End)
    end.

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