-module(day_5).
%% You'll have to read the output to get the answer

-compile(export_all).

-record(stacks, {stack1 = [],
                 stack2 = [],
                 stack3 = [],
                 stack4 = [],
                 stack5 = [],
                 stack6 = [],
                 stack7 = [],
                 stack8 = [],
                 stack9 = []}).

problem(1) ->
    {ok, Input} = file:open("input.txt", read),
    Input2 = skip_stacks(Input),
    Stacks = initial_stacks(),
    move_items(lists:reverse(read_to_list(Input2)), Stacks, 1);
problem(2) -> 
    {ok, Input} = file:open("input.txt", read),
    Input2 = skip_stacks(Input),
    Stacks = initial_stacks(),
    move_items(lists:reverse(read_to_list(Input2)), Stacks, 2).

move_items([], Stacks, _Version) ->
    Stacks;
move_items([Order | Orders], Stacks, Version) ->
    ["move", Quantity, "from", From, "to", To] = string:tokens(Order, " "),
    FromStack = element(list_to_integer(From) + 1, Stacks),
    ToStack = element(list_to_integer(To) + 1, Stacks),
    {ToMove, NewFromStack} = lists:split(list_to_integer(Quantity), FromStack),
    NewToStack = case Version of 
        1 -> lists:reverse(lists:append(lists:reverse(ToStack), ToMove));
        2 -> lists:reverse(lists:append(lists:reverse(ToStack), lists:reverse(ToMove)))
    end,
    Stacks2 = update_stacks(From, NewFromStack, Stacks),
    Stacks3 = update_stacks(To, NewToStack, Stacks2),
    io:format("Order was ~p, Stacks are ~p~n", [Order, Stacks]),
    move_items(Orders, Stacks3, Version).


update_stacks("1", NewStack, Stacks) ->
    Stacks#stacks{stack1 = NewStack};
update_stacks("2", NewStack, Stacks) ->
    Stacks#stacks{stack2 = NewStack};
update_stacks("3", NewStack, Stacks) ->
    Stacks#stacks{stack3 = NewStack};
update_stacks("4", NewStack, Stacks) ->
    Stacks#stacks{stack4 = NewStack};
update_stacks("5", NewStack, Stacks) ->
    Stacks#stacks{stack5 = NewStack};
update_stacks("6", NewStack, Stacks) ->
    Stacks#stacks{stack6 = NewStack};
update_stacks("7", NewStack, Stacks) ->
    Stacks#stacks{stack7 = NewStack};
update_stacks("8", NewStack, Stacks) ->
    Stacks#stacks{stack8 = NewStack};
update_stacks("9", NewStack, Stacks) ->
    Stacks#stacks{stack9 = NewStack}.

skip_stacks(Input) ->
    case file:read_line(Input) of
        {ok, "\n"} -> Input;
        _ -> skip_stacks(Input)
    end.


initial_stacks() ->
    #stacks{
        stack1 = ["N", "T", "B", "S", "Q", "H", "G", "R"],
        stack2 = ["J", "Z", "P", "D", "F", "S", "H"],
        stack3 = ["V", "H", "Z"],
        stack4 = ["H", "G", "F", "J", "Z", "M"],
        stack5 = ["R", "S", "M", "L", "D", "C", "Z", "T"],
        stack6 = ["J", "Z", "H", "V", "W", "T", "M"],
        stack7 = ["Z", "L", "P", "F", "T"],
        stack8 = ["S", "W", "V", "Q"],
        stack9 = ["C", "N", "D", "T", "M", "L", "H", "W"]
    }.


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