-module(eappstat).
-include("cecho.hrl").
-include("include/eappstat.hrl").

-export([start/1, capture_and_plot/2, plot/2]).

-define(HEADERHEIGHT, 2).
-define(FOOTERHEIGHT, 5).
-define(INDENT, 2).

start(Node) ->
    application:set_env(eappstat, node, Node),
    application:start(eappstat),
    Env = setup(),
    input(Node, Env).

input(Node, Env) ->
    Capture = eappstat_capture:capture(),
    plot(Capture, Env),
    input(Node, Capture, Env).

input(Node, Capture, Env) ->
    {CaptureNew, EnvNew} =
    case [cecho:getch()] of
        [10] ->
            % capture
            {capture_and_plot(Capture, Env), Env};
         "r" ->
            switch_mode(reductions, Capture, Env);
         "m" ->
            switch_mode(memory, Capture, Env);
         "q" ->
            switch_mode(message_queue_len, Capture, Env);
         "1" ->
            switch_mode(reductions, Capture, Env);
         "2" ->
            switch_mode(memory, Capture, Env);
         "3" ->
            switch_mode(message_queue_len, Capture, Env);
         [66] ->
            % down
            EnvDown = Env#env{ cursor_y = Env#env.cursor_y + 1 },
            EnvPlot = plot(Capture, EnvDown),
            {Capture, adjust_shift_y(EnvPlot)};
         [65] ->
            % up
            EnvUp   = Env#env{ cursor_y = max(1, Env#env.cursor_y - 1) },
            EnvPlot = plot(Capture, adjust_shift_y(EnvUp)),
            {Capture, EnvPlot};
         "D" ->
            % left
            PrevCapture = eappstat_capture:prev_capture(),
            EnvPlot     = plot(PrevCapture, Env),
            {PrevCapture, EnvPlot};
         "C" ->
            % right
            NextCapture = eappstat_capture:next_capture(),
            EnvPlot     = plot(NextCapture, Env),
            {NextCapture, EnvPlot};
         " " ->
            % we toggle during plotting
            EnvPlot  = plot(Capture, Env#env{ toggle_open = true }),
            EnvPlot2 = plot(Capture, EnvPlot),
            {Capture, EnvPlot2#env{ toggle_open = false }};
        _Else ->
            {Capture, Env}
    end,
    input(Node, CaptureNew, EnvNew).

switch_mode(Mode, Capture, Env) ->
    EnvMode = Env#env{ mode = Mode },
    EnvPlot = plot(Capture, EnvMode),
    {Capture, EnvPlot}.


adjust_shift_y(Env = #env{ cursor_y = CursorY, shift_y = ShiftY, body_height = BodyHeight }) ->
    case CursorY of
        _ when CursorY == (BodyHeight + ShiftY - 1) ->
            Env#env{ shift_y = Env#env.shift_y + 1 };
        _ when CursorY == (ShiftY) ->
            Env#env{ shift_y = Env#env.shift_y - 1 };
        _ ->
            Env
    end.


setup() ->
    ok = lager:start(),
    lager:set_loglevel(lager_console_backend, error),
    ok = application:start(cecho),
    cecho:cbreak(),
    cecho:noecho(),
    cecho:start_color(),
    cecho:init_color(?CURSORCOLOR, 700, 700, 700),

    cecho:init_color(?BLACK, 0, 0, 0),
    cecho:init_color(?WHITE, 800, 800, 800),
    cecho:init_color(?GREEN, 100, 1000, 100),
    cecho:init_color(?DARKGRAY, 150, 150, 150),

    cecho:init_color(?RED, 1000, 300, 50),
    cecho:init_color(?YELLOW, 800, 800, 50),
    cecho:init_color(?BLUE, 300, 300, 800),
    PaleFactor = 1/3,
    cecho:init_color(?RED_PALE,    trunc(1000 * PaleFactor), trunc(300 * PaleFactor), trunc(50 * PaleFactor)),
    cecho:init_color(?YELLOW_PALE, trunc(800 * PaleFactor), trunc(800 * PaleFactor), trunc(50 * PaleFactor)),
    cecho:init_color(?BLUE_PALE,   trunc(300 * PaleFactor), trunc(300 * PaleFactor), trunc(800 * PaleFactor)),


    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:init_pair(?WHITE_TYPE, ?WHITE, ?BLACK),
    cecho:init_pair(?POOL_TYPE, ?WHITE, ?DARKGRAY),
    cecho:init_pair(?RED_TYPE, ?RED, ?BLACK),
    cecho:init_pair(?YELLOW_TYPE, ?YELLOW, ?BLACK),
    cecho:init_pair(?BLUE_TYPE, ?BLUE, ?BLACK),
    cecho:init_pair(?RED_PALE_TYPE, ?RED_PALE, ?BLACK),
    cecho:init_pair(?YELLOW_PALE_TYPE, ?YELLOW_PALE, ?BLACK),
    cecho:init_pair(?BLUE_PALE_TYPE, ?BLUE_PALE, ?BLACK),
    cecho:init_pair(?WHITE_HL_TYPE, ?DARKGRAY, ?WHITE),
    cecho:init_pair(?CURSOR_HL, ?BLACK, ?CURSORCOLOR),
    {YMax, XMax} = cecho:getmaxyx(),
    Header       = cecho:newwin(?HEADERHEIGHT, XMax, 0, 0),
    BodyHeight   = YMax - ?HEADERHEIGHT - ?FOOTERHEIGHT,
    Body         = cecho:newwin(BodyHeight, XMax, ?HEADERHEIGHT, 0),
    Footer       = cecho:newwin(?FOOTERHEIGHT, XMax, ?HEADERHEIGHT + BodyHeight, 0),
    cecho:keypad(Body, true),
    cecho:scrollok(Body, true),
    #env{ mode = reductions, cursor_y = 0, shift_y = 0, open_pids = sets:new(), header = Header, body = Body, footer = Footer, body_height = BodyHeight}.

capture_and_plot(CaptureOld, Env) ->
    plot(CaptureOld, Env#env{ capturing = true }),
    Capture = eappstat_capture:capture(),
    plot(Capture, Env),
    Capture.

%%%%%%%%%%%%%%%%%%%%%%% plotting

plot(Capture = #capture{ tree = Tree, totals = Totals }, Env) ->
    cecho:werase(Env#env.body),
    EnvStats = Env#env{ totals = Totals },
    plot_header(EnvStats, Capture),
    EnvPlot = plot_body(Tree, EnvStats),
    plot_footer(),
    EnvPlot.

plot_header(Env, Capture) ->
    eappstat_header:set_env(Env),
    eappstat_header:set_capture(Capture),
    eappstat_header:plot().

plot_footer() ->
    eappstat_footer:plot().

plot_body(Tree, Env = #env{ body = Body } ) ->
    Result = plot_table(undefined, Tree, Env#env{ x = 1, y = 1 }),
    cecho:wrefresh(Body),
    Result.

plot_table(Parent, Node = #node{ type = supervisor }, Env = #env{ y = Y, body_height = BodyHeight, shift_y = ShiftY }) ->
    case Y >= (BodyHeight + ShiftY) of
        true ->
            Env;
        false ->
            EnvCurrentSupPid = Env#env{ current_sup_pid = proplists:get_value(pid, Node#node.proc_info) },
            maybe_highlight(fun() -> print(Parent, Node, EnvCurrentSupPid) end, Node, EnvCurrentSupPid),
            NewEnv = EnvCurrentSupPid#env{ y = Y + 1, x = EnvCurrentSupPid#env.x + ?INDENT },
            case is_pool(Node#node.children, NewEnv) of
                true ->
                    PoolEnv = plot_pool(Node#node.children, maybe_open(NewEnv#env{ x = NewEnv#env.x })),
                    PoolEnv#env{ x = NewEnv#env.x - ?INDENT };
                false ->
                    Children = sort_by_reductions(Node#node.children),
                    ChildEnv = lists:foldl(fun(Child, FoldEnv) -> plot_table(Node, Child, maybe_open(FoldEnv)) end, NewEnv, Children),
                    ChildEnv#env{ x = NewEnv#env.x - ?INDENT }
            end
    end;


plot_table(Parent, Node, Env = #env{ y = Y, body_height = BodyHeight, shift_y = ShiftY }) ->
    case Y >= (BodyHeight + ShiftY) of
        true ->
            Env;
        false ->
            maybe_highlight(fun() -> print(Parent, Node, Env) end, Node, Env),
            NewEnv =  Env#env{ y = Y + 1, x = Env#env.x + ?INDENT },
            ChildEnv = lists:foldl(fun(Child, FoldEnv) -> maybe_open(plot_table(Node, Child, FoldEnv)) end, NewEnv, Node#node.children),
            ChildEnv#env{ x = NewEnv#env.x - ?INDENT}
    end.



plot_pool(Members, Env) ->
    Values          = [eappstat_utils:total(Env#env.mode, Member) || Member <- Members],
    Balance         = eappstat_utils:balance(Values),
    TotalReductions = lists:sum([eappstat_utils:total(reductions, Member) || Member <- Members]),
    TotalMemory     = lists:sum([eappstat_utils:total(memory, Member) || Member <- Members]),
    TotalMsg        = lists:sum([eappstat_utils:total(message_queue_len, Member) || Member <- Members]),
    Totals          = #totals{ reductions = TotalReductions, memory = TotalMemory, message_queue_len = TotalMsg },
    case is_number(Balance) of
        true ->
            maybe_highlight_pool(
                fun() -> f(Env#env.x, Env#env.y - Env#env.shift_y, "p: procs: ~p balance: ~.1f %", [length(Members), Balance * 100], Env) end,
                #node{ type = pool, totals = Totals, children = Members},
                Env
             );
        false ->
            maybe_highlight_pool(
                fun() -> f(Env#env.x, Env#env.y - Env#env.shift_y, "p: procs: ~p balance: ~s", [length(Members), Balance], Env) end,
                #node{ type = pool, totals = Totals, children = Members},
                Env
            )
    end,
    Env#env{ y = Env#env.y + 1 }.

maybe_open( Env = #env{ toggle_open = true, cursor_y = CursorY, y = CursorY, current_sup_pid = CurrentSupPid, open_pids = OpenPids } ) ->
    OpenPidsNew =
    case sets:is_element(CurrentSupPid, OpenPids) of
        true ->
            sets:del_element(CurrentSupPid, OpenPids);
        false ->
            sets:add_element(CurrentSupPid, OpenPids)
    end,
    Env#env{ open_pids = OpenPidsNew, toggle_open = false };
maybe_open(Env) ->
    Env.

maybe_highlight(Fun, Node, Env = #env{ cursor_y = CursorY, y = CursorY, body = Body }) ->
    eappstat_footer:set_node(Node, Env),
    eappstat_utils:color(Body, ?CURSOR_HL),
    Fun();
maybe_highlight(Fun, _, #env{ body = Body }) ->
    eappstat_utils:color(Body, ?WHITE_TYPE),
    Fun().

maybe_highlight_pool(Fun, Node, Env = #env{ cursor_y = CursorY, y = CursorY, body = Body }) ->
    eappstat_footer:set_node(Node, Env),
    eappstat_utils:color(Body, ?CURSOR_HL),
    Fun();
maybe_highlight_pool(Fun, _, #env{ body = Body }) ->
    eappstat_utils:color(Body, ?POOL_TYPE),
    Fun().

print(_, Node = #node{ type = node }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "n: ~s ", [Node#node.name], {totals(Env#env.mode, Env), totals(Env#env.mode, Env), totals(Env#env.mode, Env)}, Env);
print(Parent, Node = #node{ type = application }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "a: ~s ", [Node#node.name], {totals(Env#env.mode, Env), eappstat_utils:total(Env#env.mode, Node), eappstat_utils:total(Env#env.mode, Parent)}, Env);
print(Parent, Node = #node{ type = supervisor }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "s: ~s ", [Node#node.name], {totals(Env#env.mode, Env), eappstat_utils:total(Env#env.mode, Node), eappstat_utils:total(Env#env.mode, Parent)}, Env);
print(Parent, Node = #node{ type = worker }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "w: ~s ", [Node#node.name], {totals(Env#env.mode, Env), eappstat_utils:total(Env#env.mode, Node), eappstat_utils:total(Env#env.mode, Parent)}, Env);
print(Parent, Node = #node{ type = process }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "p: ~s ", [Node#node.name], {totals(Env#env.mode, Env), eappstat_utils:total(Env#env.mode, Node), eappstat_utils:total(Env#env.mode, Parent)}, Env);
print(_, #node{ type = Type }, _) ->
    lager:info("unkonwn type: ~p\n", [Type]).

is_pool([_], _) ->
    false;
is_pool(Members, #env{ current_sup_pid = CurrentSupPid, open_pids = OpenPids } ) ->
    case sets:is_element(CurrentSupPid, OpenPids) of
        true ->
            false;
        false ->
            AllWorkers = lists:all(fun(M) -> M#node.type == worker end, Members),
            InitialCalls =
            [proplists:get_value('$initial_call', proplists:get_value(dictionary, Member#node.proc_info))
             || Member <- Members],
            AllSameCalls = length(lists:usort(InitialCalls)) == 1,
            AllWorkers and AllSameCalls
    end.

totals(reductions, Env) ->
    Env#env.totals#totals.reductions;
totals(memory, Env) ->
    Env#env.totals#totals.memory;
totals(message_queue_len, Env) ->
    Env#env.totals#totals.message_queue_len.

sort_by_reductions(Nodes) ->
    lists:sort(
        fun(A, B) -> eappstat_utils:total(reductions, A) > eappstat_utils:total(reductions, B) end,
        Nodes
     ).

f(_, Y, _, _, _) when Y < 1 ->
    noop;
f(X, Y, String, Args, Env) ->
    Body = Env#env.body,
    case move_if_ok(Y, X, Body) of
        ok ->
            cecho:waddstr(Body, io_lib:format(eappstat_utils:fnorm(String, Args) ++ "\n", [])),
            blank_spacer(Body, Y);
        not_ok ->
            noop
    end.

f(_, Y, _, _, _, _) when Y < 1 ->
    noop;
f(X, Y, String, Args, {0, _, _}, Env) ->
    Body = Env#env.body,
    Left  = eappstat_utils:fnorm(String, Args),
    case move_if_ok(Y, X, Body) of
        ok ->
            cecho:waddstr(Body, Left),
            blank_spacer(Body, Y);
        _ ->
            noop
    end;
f(X, Y, String, Args, {AppLoad, ProcLoad, ParentLoad}, Env) ->
    Body = Env#env.body,
    Mode = Env#env.mode,
    {_, XMax}   = cecho:getmaxyx(Body),
    Left  = eappstat_utils:fnorm(String, Args),
    FractTotal  = ProcLoad / AppLoad,
    {RightParent, FractParent} =
    case (ParentLoad > 0) and plot_fract_parent(Mode) of
        false ->
            {undefined, undefined};
        true ->
            FractP = ProcLoad / ParentLoad,
            BarP   = trunc(FractP * (XMax - 57)),
            {
                ["|" || _ <- lists:seq(1, BarP)] ++ format_number(ProcLoad, ParentLoad, Mode),
                FractP
            }
    end,
    BarTotal    = trunc(FractTotal  * (XMax - 57)),
    RightTotal = ["|" || _ <- lists:seq(1, BarTotal)] ++ format_number(ProcLoad, AppLoad, Mode),
    case move_if_ok(Y, X, Body) of
        ok ->
            cecho:waddstr(Body, Left),
            eappstat_utils:color(Body, ?WHITE_TYPE),
            blank_spacer(Body, Y),
            case RightParent of
                undefined ->
                    noop;
                _ ->
                    load_color_pale(FractParent, Body),
                    cecho:wmove(Body, Y, 50),
                    cecho:waddstr(Body, RightParent)
            end,
            load_color(FractTotal, Body),
            cecho:wmove(Body, Y, 50),
            cecho:waddstr(Body, RightTotal);
        not_ok ->
            noop
    end.

blank_spacer(Window, Y) ->
    eappstat_utils:color(Window, ?WHITE_TYPE),
    cecho:wmove(Window, Y, 49),
    cecho:waddstr(Window, "                ").

format_number(Value, Reference, reductions) ->
    io_lib:format("~.1f%", [(Value / Reference) * 100]);
format_number(Value, _Reference, memory) ->
    {MemoryValue, MemoryOOM} = eappstat_utils:oom(Value, 1024),
    io_lib:format("~.1f ~sB", [MemoryValue, MemoryOOM]);
format_number(Value, _Reference, message_queue_len) ->
    io_lib:format("~p", [Value]).


plot_fract_parent(reductions) -> true;
plot_fract_parent(_)          -> false.

move_if_ok(X, Y, Window) ->
    {YMax, _XMax} = cecho:getmaxyx(),
    case (Y + ?HEADERHEIGHT) < YMax of
        true ->
            cecho:wmove(Window, X, Y),
            ok;
        false ->
            not_ok
    end.


load_color(Fract, Window) ->
    Color =
    case Fract of
        _ when Fract < 0.25 -> ?BLUE_TYPE;
        _ when Fract < 0.50 -> ?YELLOW_TYPE;
        _ -> ?RED_TYPE
    end,
    eappstat_utils:color(Window, Color).

load_color_pale(Fract, Window) ->
    Color =
    case Fract of
        _ when Fract < 0.25 -> ?BLUE_PALE_TYPE;
        _ when Fract < 0.50 -> ?YELLOW_PALE_TYPE;
        _ -> ?RED_PALE_TYPE
    end,
    eappstat_utils:color(Window, Color).
