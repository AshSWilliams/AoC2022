-module(day_6).

-compile(export_all).

problem(1) ->
    {ok, Input} = file:open("input.txt", read),
    {ok, Line} = file:read_line(Input),
    LineList = [[X] || X <- Line],
    get_start_marker(LineList, 1, 4);
problem(2) -> 
    {ok, Input} = file:open("input.txt", read),
    {ok, Line} = file:read_line(Input),
    LineList = [[X] || X <- Line],
    get_start_marker(LineList, 1, 14).

get_start_marker(LineList, Pos, Len) ->
    Slice = lists:sublist(LineList, Pos, Len),
    SetSlice = sets:from_list(Slice),
    case element(2, SetSlice) < Len of
        true ->
            get_start_marker(LineList, Pos + 1, Len);
        false ->
            Pos + Len - 1
    end.

