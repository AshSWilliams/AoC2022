-module(day_1).

-compile(export_all).

problem(1) ->
    {ok, Input} = file:open("input.txt", read),
    CalorieCount = count_calories(Input, []),
    lists:nth(1, CalorieCount);
problem(2) -> 
    {ok, Input} = file:open("input.txt", read),
    CalorieCount = count_calories(Input, []),
    lists:nth(1, CalorieCount) + lists:nth(2, CalorieCount) + lists:nth(3, CalorieCount).

count_calories(Input, []) ->
    case file:read_line(Input) of
        {ok, "\n"} ->
            {error, blank_start};
        {ok, Data} ->
            Calories = list_to_integer(string:trim(Data, trailing, "\n")),
            count_calories(Input, [Calories]);
        eof ->
            {error, blank_start}
    end;
count_calories(Input, [Calories | Cons] = CalorieCount) ->
    case file:read_line(Input) of 
        {ok, "\n"} ->
            count_calories(Input, [0 | CalorieCount]);
        {ok, Data} ->
            NewCalories = list_to_integer(string:trim(Data, trailing, "\n")),
            count_calories(Input, [Calories + NewCalories | Cons]);
        eof ->
            lists:reverse(lists:sort(CalorieCount))
    end.
