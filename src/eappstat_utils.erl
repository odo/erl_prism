-module(eappstat_utils).
-include("cecho.hrl").
-include("include/eappstat.hrl").

-compile([export_all]).

f(X, Y, String, Args, Window) ->
    cecho:wmove(Window, Y, X),
    cecho:waddstr(Window, io_lib:format(eappstat_utils:fnorm(String, Args) ++ "\n", [])).

total(Type, #node{ proc_info = ProcInfo, children = Children }) ->
    Increment =
    case ProcInfo of
        undefined -> 0;
        _         -> proplists:get_value(Type, ProcInfo)
    end,
    Increment + lists:sum([total(Type, Child) || Child <- Children]).

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


maybe_hl(Mode, Mode, Window) ->
    color(Window, ?WHITE_HL_TYPE);
maybe_hl(_, _, Window) ->
    color(Window, ?WHITE_TYPE).

balance(Reductions) ->
    case lists:sum(Reductions) of
        0 ->
            "-";
        _ ->
            1 - eappstat_gini:index(Reductions)
    end.

