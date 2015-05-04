-module(erl_prism_gini).

-export([index/1]).

index(Values) ->
    ValuesSort = lists:sort(Values),
    Length     = length(ValuesSort),
    Mean       = mean(ValuesSort),
    Loop       = loop(ValuesSort, 1, Length),
    case Mean of
        0.0 ->
            0.0;
        _ ->
            Loop / (math:pow(Length, 2) * Mean)
    end.

loop([], _, _) ->
    0;
loop([X | Rest], Index, Length) ->
    (2 * Index - Length - 1) * X +
    loop(Rest, Index + 1, Length).

mean(Values) ->
    lists:sum(Values) / length(Values).
