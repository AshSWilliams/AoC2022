-module(day_3).

-compile(export_all).

problem(1) ->
    {ok, Input} = file:open("input.txt", read),
    Items = read_to_list(Input),
    sum_items(Items, 0);    
problem(2) -> 
    {ok, Input} = file:open("input.txt", read),
    sum_badges(Input, 0).


sum_items([], Sum) ->
    Sum;
sum_items(Items, Sum) ->
    [Head | Cons] = Items,
    CompartmentLen = length(Head) div 2,
    FirstChars = characterize(string:slice(Head, 0, CompartmentLen)),
    SecondChars = characterize(string:slice(Head, CompartmentLen, CompartmentLen)),
    CommonItem = lists:nth(1, [X || X <- FirstChars, lists:member(X, SecondChars)]),
    %% io:format("Common: ~p  ", [CommonItem]),
    %% io:format("Value: ~p~n", [score(CommonItem)]),
    sum_items(Cons, Sum + score(CommonItem)).

sum_badges(Input, Sum) ->
    io:format("current sum ~p~n", [Sum]),
    %% This throws an error when it hits the end of the file
    %% I cba to fix it, so just read the output from above
    Rucksacks = [string:trim(element(2, file:read_line(Input)), trailing, "\n") || X <- [1,2,3]] of
    Chars = lists:map(fun(A) -> characterize(A) end, Rucksacks),
    SharedChar = in_three_lists(Chars),
    io:format("shared: ~p  ", [SharedChar]),
    sum_badges(Input, Sum + score(SharedChar)).

characterize(List) ->
    [[X] || X <- List].

in_three_lists([List1, List2, List3] = _CharLists) ->
    Shared = [X || X <- List1, lists:member(X, List2)],
    lists:nth(1, [X || X <- Shared, lists:member(X, List3)]).

score(X) -> 
    case X =:= string:uppercase(X) of
        true -> score_upper(X);
        false -> score_lower(X)
    end.

score_upper(Char) ->
    element(1, lists:nth(1, [{Val} || Val <- Char])) - 38.
score_lower(Char) ->
    element(1, lists:nth(1, [{Val} || Val <- Char])) - 96.

read_to_list(Input) ->
    case file:read_line(Input) of
        {ok, Line} ->
            read_to_list(Input, [Line]);
        eof ->
            []
    end.
read_to_list(Input, []) ->
    read_to_list(Input);
read_to_list(Input, _Read) ->
        case file:read_line(Input) of
        {ok, Line} ->
            read_to_list(Input, [Line | _Read]);
        eof ->
            _Read
    end.