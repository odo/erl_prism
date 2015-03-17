-module(eappstat_utils).
-include("cecho.hrl").

-compile([export_all]).

color(Window, Color) ->
    color(Window, Color, false).

color(Window, Color, false) ->
    cecho:attron(Window, ?ceA_NORMAL bor ?ceCOLOR_PAIR(Color));
color(Window, Color, true) ->
    cecho:attron(Window, ?ceA_BOLD bor ?ceCOLOR_PAIR(Color)).

fnorm(String, Args) ->
    binary_to_list(iolist_to_binary(io_lib:format(String, Args))).

oom(Value, Base) ->
    {ValueNew, Depth} = oom(Value, 0, Base),
    {ValueNew, oom(Depth)}.

oom(Value, Depth, Base) ->
    case Value < Base of
        true  ->
            {Value * 1.0, Depth};
        false ->
            NewDepth = Depth + 1,
            NewValue = Value / Base,
            oom(NewValue, NewDepth, Base)
    end.

oom(0) -> "";
oom(1) -> "k";
oom(2) -> "M";
oom(3) -> "G";
oom(4) -> "T";
oom(_) -> "XXL".
