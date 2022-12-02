-module(day_2).

-compile(export_all).



problem(1) ->
    {ok, Input} = file:open("input.txt", read),
    calculate_score(Input, move);
problem(2) ->
    {ok, Input} = file:open("input.txt", read),
    calculate_score(Input, outcome).

calculate_score(Input, Style) ->
    {ok, Line} = file:read_line(Input),
    [Them, Us] = parse_line(Line),
    Score = do_calculate_score(Them, Us, Style),
    calculate_score(Input, Score, Style).
calculate_score(Input, Score, Style) ->
    case file:read_line(Input) of
        {ok, Line} ->
            [Them, Us] = parse_line(Line),
            New_Score = do_calculate_score(Them, Us, Style),
            calculate_score(Input, Score + New_Score, Style);
        eof ->
            Score
    end.

do_calculate_score("A", "X", move) -> 4;
do_calculate_score("A", "Y", move) -> 8;
do_calculate_score("A", "Z", move) -> 3;
do_calculate_score("B", "X", move) -> 1;
do_calculate_score("B", "Y", move) -> 5;
do_calculate_score("B", "Z", move) -> 9;
do_calculate_score("C", "X", move) -> 7;
do_calculate_score("C", "Y", move) -> 2;
do_calculate_score("C", "Z", move) -> 6;
do_calculate_score("A", "X", outcome) -> 3;
do_calculate_score("A", "Y", outcome) -> 4;
do_calculate_score("A", "Z", outcome) -> 8;
do_calculate_score("B", "X", outcome) -> 1;
do_calculate_score("B", "Y", outcome) -> 5;
do_calculate_score("B", "Z", outcome) -> 9;
do_calculate_score("C", "X", outcome) -> 2;
do_calculate_score("C", "Y", outcome) -> 6;
do_calculate_score("C", "Z", outcome) -> 7.


parse_line(Line) ->
    string:tokens(string:trim(Line, trailing, "\n"), " ").