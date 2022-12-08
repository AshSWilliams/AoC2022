-module(day_7).

-compile(export_all).

problem(N) ->
    {ok, Input} = file:open("input.txt", read),
    InputList = lists:reverse(read_to_list(Input)),
    TokenInput = lists:map(fun(A) -> string:tokens(A, " ") end, InputList),
    DictOfSizes = traverse_dirs(TokenInput, dict:new(), []),
    RootSize = dict:fetch("/", DictOfSizes),
    case N of
        1 ->
            SmallDicts = dict:filter(fun(_K,V) -> V < 100001 end, DictOfSizes),
            dict:fold(fun(_K,V,Acc) -> Acc + V end, 0, SmallDicts);
        2 ->
            FilterDicts = dict:filter(fun(_K,V) -> V > RootSize - 40000000 end, DictOfSizes),
            lists:min([ dict:fetch(X, FilterDicts) || X <- dict:fetch_keys(FilterDicts)])
    end.

traverse_dirs([], Dict, _Pos) ->
    Dict;
traverse_dirs([["$", "cd", "/"] | Rest], Dict, _Pos) ->
    %io:format("returning to root~n", []),
    traverse_dirs(Rest, Dict, ["/"]);
traverse_dirs([["$", "cd", ".."] | Rest], Dict, Pos) ->
    %io:format("going up one level, current position ~p~n", [Pos]),
    traverse_dirs(Rest, Dict, lists:sublist(Pos, 1, length(Pos) - 1));
traverse_dirs([["$", "cd", NewDir] | Rest], Dict, Pos) ->
    %io:format("moving into directory ~p, current position~p~n", [NewDir, Pos]),
    NewPos = lists:nth(length(Pos), Pos) ++ NewDir ++ "/",
    traverse_dirs(Rest, Dict, lists:append(Pos, [NewPos]));
traverse_dirs([["$", "ls"] | Rest], Dict, Pos) ->
    traverse_dirs(Rest, Dict, Pos);
traverse_dirs([["dir", _] | Rest], Dict, Pos) ->
    traverse_dirs(Rest, Dict, Pos);
traverse_dirs([[Size, _] | Rest], Dict, Pos) ->
    % io:format("updating size, position ~p~n", [Pos]),
    traverse_dirs(Rest, update_sizes(Dict, list_to_integer(Size), Pos), Pos).

update_sizes(Dict, _Size, []) ->
    Dict;
update_sizes(Dict, Size, [Dir | Dirs]) ->
    case dict:is_key(Dir, Dict) of
        true ->
            OldSize = dict:fetch(Dir, Dict),
            NewSize = OldSize + Size,
            NewDict = dict:store(Dir, NewSize, Dict),
            % io:format("existing directory ~p had size ~p, file size ~p, new size ~p~n", [Dir, OldSize, Size, NewSize]),
            update_sizes(NewDict, Size, Dirs);
        false ->
            % io:format("new directory ~p has new size ~p~n", [Dir, Size]),
            update_sizes(dict:store(Dir, Size, Dict), Size, Dirs)
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