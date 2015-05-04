-module(erl_prism_utils).
-include("cecho.hrl").
-include("include/erl_prism.hrl").

-compile([export_all]).

f(X, Y, String, Args, Window) ->
    cecho:wmove(Window, Y, X),
    cecho:waddstr(Window, io_lib:format(erl_prism_utils:fnorm(String, Args) ++ "\n", [])).

total(reductions, Node) ->
    Node#node.totals#totals.reductions;
total(memory, Node) ->
    Node#node.totals#totals.memory;
total(message_queue_len, Node) ->
    Node#node.totals#totals.message_queue_len.

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
            1 - erl_prism_gini:index(Reductions)
    end.

