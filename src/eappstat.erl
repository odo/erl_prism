-module(eappstat).
-include("cecho.hrl").
-include("include/eappstat.hrl").

-export([start/1, capture_and_plot/2, plot/2]).

-define(BLACK, 0).
-define(WHITE, 1).
-define(RED, 2).
-define(GREEN, 3).
-define(BLUE, 4).
-define(YELLOW, 5).

-define(WHITE_TYPE, 101).
-define(GREEN_TYPE, 102).
-define(YELLOW_TYPE, 103).
-define(RED_TYPE, 104).
-define(WHITE_HL_TYPE, 105).
-define(GREEN_HL_TYPE, 106).
-define(YELLOW_HL_TYPE, 107).
-define(RED_HL_TYPE, 108).
-define(BLUE_TYPE, 109).
-define(BLUE_HL_TYPE, 110).
-define(CURSORCOLOR, 111).
-define(CURSOR_HL, 111).

-define(HEADERHEIGHT, 4).
-define(FOOTERHEIGHT, 4).
-define(INDENT, 2).

start(Node) ->
    Env = setup(),
    input(Node, Env).

input(Node, Env) ->
    Capture = eappstat_capture:capture(Node),
    plot(Capture, Env),
    input(Node, Capture, Env).

input(Node, Capture, Env) ->
    {CaptureNew, EnvNew} =
    case [cecho:getch()] of
        [10] ->
            % capture
            {capture_and_plot(Node, Env), Env};
         [66] ->
            % down
            EnvDown = Env#env{ cursor_y = Env#env.cursor_y + 1 },
            EnvPlot = plot(Capture, EnvDown),
            {Capture, adjust_shift_y(EnvPlot)};
         [65] ->
            % up
            EnvUp = Env#env{ cursor_y = max(1, Env#env.cursor_y - 1) },
            EnvPlot = plot(Capture, adjust_shift_y(EnvUp)),
            {Capture, EnvPlot};
         " " ->
            % we toggle during plotting
            EnvPlot  = plot(Capture, Env#env{ toggle_open = true }),
            EnvPlot2 = plot(Capture, EnvPlot),
            {Capture, EnvPlot2#env{ toggle_open = false }};
        _Else ->
            {Capture, Env}
    end,
    input(Node, CaptureNew, EnvNew).

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
    cecho:init_color(?RED, 1000, 300, 50),
    cecho:init_color(?GREEN, 100, 1000, 100),
    cecho:init_color(?BLUE, 300, 300, 800),
    cecho:init_color(?YELLOW, 800, 800, 50),

    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:init_pair(?WHITE_TYPE, ?WHITE, ?BLACK),
    cecho:init_pair(?YELLOW_TYPE, ?YELLOW, ?BLACK),
    cecho:init_pair(?BLUE_TYPE, ?BLUE, ?BLACK),
    cecho:init_pair(?RED_TYPE, ?RED, ?BLACK),
    cecho:init_pair(?CURSOR_HL, ?BLACK, ?CURSORCOLOR),
    {YMax, XMax} = cecho:getmaxyx(),
    Header       = cecho:newwin(?HEADERHEIGHT, XMax, 0, 0),
    BodyHeight   = YMax - ?HEADERHEIGHT - ?FOOTERHEIGHT,
    Body         = cecho:newwin(BodyHeight, XMax, ?HEADERHEIGHT, 0),
    Footer       = cecho:newwin(?FOOTERHEIGHT, XMax, ?HEADERHEIGHT + BodyHeight, 0),
    cecho:keypad(Body, true),
    cecho:scrollok(Body, true),
    #env{ mode = reductions, cursor_y = 0, shift_y = 0, open_pids = sets:new(), header = Header, body = Body, footer = Footer, body_height = BodyHeight}.

capture_and_plot(Node, Env) ->
    Capture = eappstat_capture:capture(Node),
    plot(Capture, Env),
    Capture.

%%%%%%%%%%%%%%%%%%%%%%% plotting

plot(Capture = #capture{ tree = Tree }, Env) ->
    cecho:werase(Env#env.header),
    cecho:werase(Env#env.body),
    cecho:werase(Env#env.footer),
    cecho:erase(),
    EnvReds = Env#env{ node_stats = node_stats(Tree) },
    plot_header(Capture, EnvReds),
    EnvPlot = plot_body(Tree, EnvReds),
    plot_footer(EnvPlot),
    EnvPlot.

plot_header(#capture{ tree = Tree, time = Time }, #env{ header = Header, node_stats = #node_stats{ total_reductions = TotalReductions } }) ->
    {{Y, M, D}, {Hr, Min, Sec}} = calendar:now_to_universal_time(Time),
    f(1, 1, "~s@~p-~p-~pT~p:~p:~p", [Tree#node.name, Y, M, D, Hr, Min, Sec], Header),
    f(1, 2, "total reductions/s: ~p", [TotalReductions], Header),
    {_, XMax} = cecho:getmaxyx(),
    HLine = ["." || _ <- lists:seq(1, XMax)],
    f(1, 3, HLine, [], Header),
    cecho:wrefresh(Header).

plot_body(Tree, Env = #env{ body = Body } ) ->
    Result = plot_table(Tree, Env#env{ x = 1, y = 1 }),
    cecho:wrefresh(Body),
    Result.

plot_footer(#env{ marked_node = undefined }) ->
    noop;
plot_footer(#env{ footer = Footer, marked_node = Node = #node{ proc_info = undefined } }) ->
    f(1, 1, "~p", [Node#node.name], Footer);
plot_footer(#env{ footer = Footer, marked_node = Node = #node{ proc_info = ProcInfo } }) ->
    Pid               = proplists:get_value(pid, ProcInfo),
    {Mod, Fun, Arity} = proplists:get_value(current_function, ProcInfo),
    Status            = proplists:get_value(status, ProcInfo),
    QueueLength       = proplists:get_value(message_queue_len, ProcInfo),
    Memory            = proplists:get_value(memory, ProcInfo),
    f(1, 1, "~s with ~p is ~p at ~p:~p/~p with ~p B and ~p msgs", [Node#node.name, Pid, Status, Mod, Fun, Arity, Memory, QueueLength], Footer),
    cecho:wrefresh(Footer).

node_stats(Tree) ->
    #node_stats{
      total_reductions = total_reductions(Tree)
    }.

total_reductions(#node{ proc_info = ProcInfo, children = Children }) ->
    Increment =
    case ProcInfo of
        undefined -> 0;
        _         -> proplists:get_value(reductions, ProcInfo)
    end,
    Increment + lists:sum([total_reductions(Child) || Child <- Children]).

plot_table(Node = #node{ type = supervisor }, Env = #env{ y = Y, body_height = BodyHeight, shift_y = ShiftY }) ->
    case Y >= (BodyHeight + ShiftY) of
        true ->
            Env;
        false ->
            EnvCurrentSupPid = Env#env{ current_sup_pid = proplists:get_value(pid, Node#node.proc_info) },
            with_color(fun() -> print(Node, EnvCurrentSupPid) end, EnvCurrentSupPid),
            MarkedEnv = maybe_mark(Node, EnvCurrentSupPid),
            NewEnv = MarkedEnv#env{ y = Y + 1, x = EnvCurrentSupPid#env.x + ?INDENT },
            case is_pool(Node#node.children, NewEnv) of
                true ->
                    PoolEnv = plot_pool(Node#node.children, maybe_open(NewEnv#env{ x = NewEnv#env.x })),
                    PoolEnv#env{ x = NewEnv#env.x - ?INDENT };
                false ->
                    Children = sort_by_reductions(Node#node.children),
                    ChildEnv = lists:foldl(fun(Child, FoldEnv) -> plot_table(Child, maybe_open(FoldEnv)) end, NewEnv, Children),
                    ChildEnv#env{ x = NewEnv#env.x - ?INDENT }
            end
    end;


plot_table(Node, Env = #env{ y = Y, body_height = BodyHeight, shift_y = ShiftY }) ->
    MarkedEnv = maybe_mark(Node, Env),
    case Y >= (BodyHeight + ShiftY) of
        true ->
            MarkedEnv;
        false ->
            with_color(fun() -> print(Node, MarkedEnv) end, MarkedEnv),
            NewEnv =  MarkedEnv#env{ y = Y + 1, x = MarkedEnv#env.x + ?INDENT },
            ChildEnv = lists:foldl(fun(Child, FoldEnv) -> maybe_open(plot_table(Child, FoldEnv)) end, NewEnv, Node#node.children),
            ChildEnv#env{ x = NewEnv#env.x - ?INDENT}
    end.



plot_pool(Members, Env) ->
    Reductions = [total_reductions(Member) || Member <- Members],
    Balance    = balance(Reductions),
    case is_number(Balance) of
        true ->
            with_color(
                fun() -> f(Env#env.x, Env#env.y - Env#env.shift_y, "p: procs: ~p balance: ~.1f %", [length(Members), Balance * 100], Env#env.body) end,
                Env
             );
        false ->
            with_color(
                fun() -> f(Env#env.x, Env#env.y - Env#env.shift_y, "p: procs: ~p balance: ~s", [length(Members), Balance], Env#env.body) end,
                Env
            )
    end,
    Env#env{ y = Env#env.y + 1 }.

balance(Reductions) ->
    case lists:sum(Reductions) of
        0 ->
            "-";
        _ ->
            1 - eappstat_gini:index(Reductions)
    end.

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

maybe_mark(Node, Env = #env{ cursor_y = CursorY, y = CursorY }) ->
    Env#env{ marked_node = Node };
maybe_mark(_, Env) ->
    Env.


with_color(Fun, #env{ cursor_y = CursorY, y = CursorY, body = Body }) ->
    color(Body, ?CURSOR_HL),
    Fun(),
    color(Body, ?WHITE_TYPE);
with_color(Fun, _) ->
    Fun().

print(Node = #node{ type = node }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "n: ~s ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, Env#env.node_stats#node_stats.total_reductions}, Env#env.body);
print(Node = #node{ type = application }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "a: ~s ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, total_reductions(Node)}, Env#env.body);
print(Node = #node{ type = supervisor }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "s: ~s ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, total_reductions(Node)}, Env#env.body);
print(Node = #node{ type = worker }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "w: ~s ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, total_reductions(Node)}, Env#env.body);
print(Node = #node{ type = process }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "p: ~s ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, total_reductions(Node)}, Env#env.body);
print(#node{ type = Type }, _) ->
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

sort_by_reductions(Nodes) ->
    lists:sort(
        fun(A, B) -> total_reductions(A) > total_reductions(B) end,
        Nodes
     ).

f(_, Y, _, _, _) when Y < 1 ->
    noop;
f(X, Y, String, Args, Window) ->
    case move_if_ok(Y, X, Window) of
        ok ->
            cecho:waddstr(Window, io_lib:format(fnorm(String, Args) ++ "\n", []));
        not_ok ->
            noop
    end.

f(_, Y, _, _, _, _) when Y < 1 ->
    noop;
f(X, Y, String, Args, {AppReds, ProcReds}, Window) ->
    Left  = fnorm(String, Args),
    Fract = ProcReds / AppReds,
    {_, XMax} = cecho:getmaxyx(Window),
    Bar   = trunc(Fract * (XMax - 57)),
    Right = ["|" || _ <- lists:seq(1, Bar)] ++ io_lib:format("~.1f%", [Fract * 100]),
    case move_if_ok(Y, X, Window) of
        ok ->
            cecho:waddstr(Window, Left),
            cecho:wmove(Window, Y, 50),
            load_color(Fract, Window),
            cecho:waddstr(Window, Right),
            color(Window, ?WHITE_TYPE);
        not_ok ->
            noop
    end.

move_if_ok(X, Y, Window) ->
    {YMax, _XMax} = cecho:getmaxyx(),
    case (Y + ?HEADERHEIGHT) < YMax of
        true ->
            cecho:wmove(Window, X, Y),
            ok;
        false ->
            not_ok
    end.

fnorm(String, Args) ->
    binary_to_list(iolist_to_binary(io_lib:format(String, Args))).

color(Window, Color) ->
    color(Window, Color, false).

color(Window, Color, false) ->
    cecho:attron(Window, ?ceA_NORMAL bor ?ceCOLOR_PAIR(Color));
color(Window, Color, true) ->
    cecho:attron(Window, ?ceA_BOLD bor ?ceCOLOR_PAIR(Color)).


load_color(Fract, Window) ->
    Color =
    case Fract of
        _ when Fract < 0.333 -> ?BLUE_TYPE;
        _ when Fract < 0.666 -> ?YELLOW_TYPE;
        _ -> ?RED_TYPE
    end,
    color(Window, Color).

