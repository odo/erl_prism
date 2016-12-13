-module(erl_prism).
-include("cecho.hrl").
-include("include/erl_prism.hrl").

-export([start/3, capture_and_plot/2, plot/2]).

-define(HEADERHEIGHT, 2).
-define(FOOTERHEIGHT, 5).
-define(INDENT, 2).

start(Node, Cookie, Options) ->
    erlang:set_cookie(node(), Cookie),
    test_connection(Node),
    application:set_env(erl_prism, node, Node),
    application:set_env(erl_prism, options, Options),
    ok = application:start(erl_prism),
    Env = setup(),
    erlang:register(erl_prism, self()),
    wait_for_keystroke(),
    input(Node, Env).

test_connection(Node) ->
  case net_adm:ping(Node) of
    pong -> ok;
    pang -> throw({error, {can_not_connect, Node}})
  end.

wait_for_keystroke() ->
    WaitAndSend = fun() ->
        erl_prism ! [cecho:getch()]
    end,
    spawn(WaitAndSend).

input(Node, Env) ->
    Capture = erl_prism_capture:capture(),
    plot(Capture, Env),
    input(Node, Capture, Env).

input(Node, Capture, Env) ->

    {CaptureNew, EnvNew, AskForNewKeystroke} =
    receive
        capture ->
            % auto-capture
            {capture_and_plot(Capture, Env), Env, false};
        [10] ->
            % capture
            {capture_and_plot(Capture, Env), Env, true};
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
         "x" ->
            erl_prism_export:export(Capture, Env),
            {Capture, Env, true};
         [66] ->
            % down
            EnvDown = Env#env{ cursor_y = Env#env.cursor_y + 1 },
            EnvPlot = plot(Capture, EnvDown),
            {Capture, adjust_shift_y(EnvPlot), true};
         [65] ->
            % up
            EnvUp   = Env#env{ cursor_y = max(1, Env#env.cursor_y - 1) },
            EnvPlot = plot(Capture, adjust_shift_y(EnvUp)),
            {Capture, EnvPlot, true};
         "D" ->
            % left
            PrevCapture = erl_prism_capture:prev_capture(),
            EnvPlot     = plot(PrevCapture, Env),
            {PrevCapture, EnvPlot, true};
         "C" ->
            % right
            NextCapture = erl_prism_capture:next_capture(),
            EnvPlot     = plot(NextCapture, Env),
            {NextCapture, EnvPlot, true};
         " " ->
            % we toggle during plotting
            EnvPlot  = plot(Capture, Env#env{ toggle_open = true }),
            EnvPlot2 = plot(Capture, EnvPlot),
            {Capture, EnvPlot2#env{ toggle_open = false }, true};
        [-1] ->
            {Capture, Env, true};
        _Else ->
            lager:info("got ~p\n", [_Else]),
            {Capture, Env, true}
        after
            5 ->
                {Capture, Env, false}
    end,

    case AskForNewKeystroke of
        true  -> wait_for_keystroke();
        false -> noop
    end,

    input(Node, CaptureNew, EnvNew).

switch_mode(Mode, Capture, Env) ->
    EnvMode = Env#env{ mode = Mode },
    EnvPlot = plot(Capture, EnvMode),
    {Capture, EnvPlot, true}.


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
    #env{ mode = reductions, cursor_y = 1, shift_y = 0, open_pids = sets:new(), header = Header, body = Body, footer = Footer, body_height = BodyHeight}.

capture_and_plot(CaptureOld, Env) ->
    plot(CaptureOld, Env#env{ capturing = true }),
    Capture = erl_prism_capture:capture(),
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
    erl_prism_header:set_env(Env),
    erl_prism_header:set_capture(Capture),
    erl_prism_header:plot().

plot_footer() ->
    erl_prism_footer:plot().

plot_body(Tree, Env = #env{ body = Body } ) ->
    Result = plot_table(undefined, Tree, Env#env{ x = 1, y = 1 }),
    cecho:wrefresh(Body),
    Result.

plot_table(Parent, Node = #node{ type = supervisor, children = Children }, Env = #env{ y = Y, body_height = BodyHeight, shift_y = ShiftY }) ->
    case Y >= (BodyHeight + ShiftY) of
        true ->
            Env;
        false ->
            EnvCurrentSupPid = Env#env{ current_sup_pid = proplists:get_value(pid, Node#node.proc_info) },
            maybe_highlight(fun() -> print(Parent, Node, EnvCurrentSupPid) end, Node, EnvCurrentSupPid),
            NewEnv = EnvCurrentSupPid#env{ y = Y + 1, x = EnvCurrentSupPid#env.x + ?INDENT },
            ChildEnv = lists:foldl(fun(Child, FoldEnv) -> plot_table(Node, Child, maybe_open(FoldEnv)) end, NewEnv, Children),
            ChildEnv
    end;

plot_table(_Parent, Node = #node{ type = pool }, Env = #env{ current_sup_pid = CurrentSupPid, open_pids = OpenPids }) ->
    case sets:is_element(CurrentSupPid, OpenPids) of
        false ->
            PoolEnv = plot_pool(Node, maybe_open(Env)),
            PoolEnv#env{ x = PoolEnv#env.x - ?INDENT };
        true ->
            Children = sort_by_reductions(Node#node.children),
            EnvNew   = lists:foldl(fun(Child, FoldEnv) -> plot_table(Node, Child, maybe_open(FoldEnv)) end, Env#env{ x = Env#env.x + ?INDENT }, Children),
            EnvNew#env{ x = EnvNew#env.x - ?INDENT }
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


plot_pool(Pool, Env) ->
    Members         = Pool#node.children,
    Values          = [erl_prism_utils:total(Env#env.mode, Member) || Member <- Members],
    Balance         = erl_prism_utils:balance(Values),
    case is_number(Balance) of
        true ->
            maybe_highlight_pool(
                fun() -> f(Env#env.x, Env#env.y - Env#env.shift_y, "p: procs: ~p balance: ~.1f %", [length(Members), Balance * 100], Env) end,
                Pool,
                Env
             );
        false ->
            maybe_highlight_pool(
                fun() -> f(Env#env.x, Env#env.y - Env#env.shift_y, "p: procs: ~p balance: ~s", [length(Members), Balance], Env) end,
                Pool,
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
    erl_prism_footer:set_node(Node, Env),
    erl_prism_utils:color(Body, ?CURSOR_HL),
    Fun();
maybe_highlight(Fun, _, #env{ body = Body }) ->
    erl_prism_utils:color(Body, ?WHITE_TYPE),
    Fun().

maybe_highlight_pool(Fun, Node, Env = #env{ cursor_y = CursorY, y = CursorY, body = Body }) ->
    erl_prism_footer:set_node(Node, Env),
    erl_prism_utils:color(Body, ?CURSOR_HL),
    Fun();
maybe_highlight_pool(Fun, _, #env{ body = Body }) ->
    erl_prism_utils:color(Body, ?POOL_TYPE),
    Fun().

print(_, Node = #node{ type = node }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "n: ~p ", [Node#node.name], {totals(Env#env.mode, Env), totals(Env#env.mode, Env), totals(Env#env.mode, Env)}, Env);
print(Parent, Node = #node{ type = application }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "a: ~p ", [Node#node.name], {totals(Env#env.mode, Env), erl_prism_utils:total(Env#env.mode, Node), erl_prism_utils:total(Env#env.mode, Parent)}, Env);
print(Parent, Node = #node{ type = supervisor }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "s: ~p ", [Node#node.name], {totals(Env#env.mode, Env), erl_prism_utils:total(Env#env.mode, Node), erl_prism_utils:total(Env#env.mode, Parent)}, Env);
print(Parent, Node = #node{ type = worker }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "w: ~p ", [Node#node.name], {totals(Env#env.mode, Env), erl_prism_utils:total(Env#env.mode, Node), erl_prism_utils:total(Env#env.mode, Parent)}, Env);
print(Parent, Node = #node{ type = process }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "p: ~p ", [Node#node.name], {totals(Env#env.mode, Env), erl_prism_utils:total(Env#env.mode, Node), erl_prism_utils:total(Env#env.mode, Parent)}, Env);
print(_, #node{ type = Type }, _) ->
    lager:info("unkonwn type: ~p\n", [Type]).

totals(reductions, Env) ->
    Env#env.totals#totals.reductions;
totals(memory, Env) ->
    Env#env.totals#totals.memory;
totals(message_queue_len, Env) ->
    Env#env.totals#totals.message_queue_len.

sort_by_reductions(Nodes) ->
    lists:sort(
        fun(A, B) -> erl_prism_utils:total(reductions, A) > erl_prism_utils:total(reductions, B) end,
        Nodes
     ).

f(_, Y, _, _, _) when Y < 1 ->
    noop;
f(X, Y, String, Args, Env) ->
    Body = Env#env.body,
    case move_if_ok(Y, X, Body) of
        ok ->
            cecho:waddstr(Body, io_lib:format(erl_prism_utils:fnorm(String, Args) ++ "\n", [])),
            blank_spacer(Body, Y);
        not_ok ->
            noop
    end.

f(_, Y, _, _, _, _) when Y < 1 ->
    noop;
f(X, Y, String, Args, {0, _, _}, Env) ->
    Body = Env#env.body,
    Left  = erl_prism_utils:fnorm(String, Args),
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
    Left  = erl_prism_utils:fnorm(String, Args),
    FractTotal  = ProcLoad / AppLoad,
    {RightParent, FractParent} =
    case (ParentLoad > 0) and (ProcLoad > 0) and plot_fract_parent(Mode) of
        false ->
            {undefined, undefined};
        true ->
            FractP = ProcLoad / ParentLoad,
            BarP   = trunc(FractP * (XMax - 59)),
            {
                ["|" || _ <- lists:seq(1, BarP)] ++ format_number(ProcLoad, ParentLoad, Mode),
                FractP
            }
    end,
    BarTotal    = trunc(FractTotal  * (XMax - 59)),
    RightTotal = ["|" || _ <- lists:seq(1, BarTotal)] ++ format_number(ProcLoad, AppLoad, Mode),
    case move_if_ok(Y, X, Body) of
        ok ->
            cecho:waddstr(Body, Left),
            erl_prism_utils:color(Body, ?WHITE_TYPE),
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
    erl_prism_utils:color(Window, ?WHITE_TYPE),
    cecho:wmove(Window, Y, 49),
    cecho:waddstr(Window, "                ").

format_number(Value, Reference, reductions) ->
    io_lib:format("~.1f%", [(Value / Reference) * 100]);
format_number(Value, _Reference, memory) ->
    {MemoryValue, MemoryOOM} = erl_prism_utils:oom(Value, 1024),
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
    erl_prism_utils:color(Window, Color).

load_color_pale(Fract, Window) ->
    Color =
    case Fract of
        _ when Fract < 0.25 -> ?BLUE_PALE_TYPE;
        _ when Fract < 0.50 -> ?YELLOW_PALE_TYPE;
        _ -> ?RED_PALE_TYPE
    end,
    erl_prism_utils:color(Window, Color).
