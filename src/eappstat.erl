-module(eappstat).
-include("cecho.hrl").

-export([start/1, capture_and_plot/2, capture/1, plot/2, proc_info_async/4]).

-record(capture, {tree, time}).
-record(node, {type, name, proc_info, pid, children = []}).
-record(env, {mode, time, node_stats, header, body, footer, x, y, x_max, y_max, cursor_y, shift_y, marked_node, toggle_open, current_sup_pid, open_pids, body_height}).
-record(node_stats, {total_reductions}).

-define(WHITE, 1).
-define(GREEN, 2).
-define(YELLOW, 3).
-define(RED, 4).
-define(WHITE_HL, 5).
-define(GREEN_HL, 6).
-define(YELLOW_HL, 7).
-define(RED_HL, 8).

-define(HEADERHEIGHT, 4).
-define(FOOTERHEIGHT, 4).

start(Node) ->
    Env = setup(),
    input(Node, Env).

input(Node, Env) ->
    Capture = capture(Node),
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
            lager:info("scroll down\n"),
            Env#env{ shift_y = Env#env.shift_y + 1 };
        _ when CursorY == (ShiftY) ->
            lager:info("scroll up\n"),
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
    cecho:curs_set(?ceCURS_INVISIBLE),
    cecho:init_pair(?WHITE, ?ceCOLOR_WHITE, ?ceCOLOR_BLACK),
    cecho:init_pair(?WHITE_HL, ?ceCOLOR_WHITE, ?ceCOLOR_BLUE),
    cecho:init_pair(?YELLOW, ?ceCOLOR_YELLOW, ?ceCOLOR_BLACK),
    cecho:init_pair(?YELLOW_HL, ?ceCOLOR_YELLOW, ?ceCOLOR_BLUE),
    cecho:init_pair(?RED, ?ceCOLOR_RED, ?ceCOLOR_BLACK),
    cecho:init_pair(?RED_HL, ?ceCOLOR_RED, ?ceCOLOR_BLUE),
    {YMax, XMax} = cecho:getmaxyx(),
    Header       = cecho:newwin(?HEADERHEIGHT, XMax, 0, 0),
    BodyHeight   = YMax - ?HEADERHEIGHT - ?FOOTERHEIGHT,
    Body         = cecho:newwin(BodyHeight, XMax, ?HEADERHEIGHT, 0),
    Footer       = cecho:newwin(?FOOTERHEIGHT, XMax, ?HEADERHEIGHT + BodyHeight, 0),
    cecho:keypad(Body, true),
    cecho:scrollok(Body, true),
    #env{ mode = reductions, cursor_y = 0, shift_y = 0, open_pids = sets:new(), header = Header, body = Body, footer = Footer, body_height = BodyHeight}.

capture_and_plot(Node, Env) ->
    Capture = capture(Node),
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
plot_footer(#env{ footer = Footer, marked_node = Node }) ->
    f(1, 1, "hello footer: ~p/~p", [Node#node.name, Node#node.pid], Footer),
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
            NewEnv = MarkedEnv#env{ y = Y + 1, x = EnvCurrentSupPid#env.x + 1 },
            case is_pool(Node#node.children, NewEnv) of
                true ->
                    PoolEnv = plot_pool(Node#node.children, maybe_open(NewEnv#env{ x = NewEnv#env.x })),
                    PoolEnv#env{ x = NewEnv#env.x - 1 };
                false ->
                    Children = sort_by_reductions(Node#node.children),
                    ChildEnv = lists:foldl(fun(Child, FoldEnv) -> plot_table(Child, maybe_open(FoldEnv)) end, NewEnv, Children),
                    ChildEnv#env{ x = NewEnv#env.x - 1}
            end
    end;


plot_table(Node, Env = #env{ y = Y, body_height = BodyHeight, shift_y = ShiftY }) ->
    MarkedEnv = maybe_mark(Node, Env),
    case Y >= (BodyHeight + ShiftY) of
        true ->
            MarkedEnv;
        false ->
            with_color(fun() -> print(Node, MarkedEnv) end, MarkedEnv),
            NewEnv =  MarkedEnv#env{ y = Y + 1, x = MarkedEnv#env.x + 1 },
            ChildEnv = lists:foldl(fun(Child, FoldEnv) -> maybe_open(plot_table(Child, FoldEnv)) end, NewEnv, Node#node.children),
            ChildEnv#env{ x = NewEnv#env.x - 1}
    end.



plot_pool(Members, Env) ->
    Reductions = [total_reductions(Member) || Member <- Members],
    Balance    = rank_fraction_half_cdf(Reductions),
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

maybe_open( Env = #env{ toggle_open = true, cursor_y = CursorY, y = CursorY, current_sup_pid = CurrentSupPid, open_pids = OpenPids } ) ->
    OpenPidsNew =
    case sets:is_element(CurrentSupPid, OpenPids) of
        true ->
            lager:info("close ~p\n", [CurrentSupPid]),
            sets:del_element(CurrentSupPid, OpenPids);
        false ->
            lager:info("open ~p\n", [CurrentSupPid]),
            sets:add_element(CurrentSupPid, OpenPids)
    end,
    lager:info("OpenPids: ~p\n", [sets:to_list(OpenPidsNew)]),
    Env#env{ open_pids = OpenPidsNew, toggle_open = false };
maybe_open(Env) ->
    Env.

maybe_mark(Node, Env = #env{ cursor_y = CursorY, y = CursorY }) ->
    lager:info("mark"),
    Env#env{ marked_node = Node };
maybe_mark(_, Env = #env{ cursor_y = CursorY, y = Y }) ->
    lager:info("~p/~p", [Y, CursorY]),
    Env.


with_color(Fun, #env{ cursor_y = CursorY, y = CursorY, body = Body, current_sup_pid = CurrentSupPid }) ->
    color(Body, ?WHITE_HL),
    lager:info("CurrentSupPid:~p\n", [CurrentSupPid]),
    Fun(),
    color(Body, ?WHITE);
with_color(Fun, _) ->
    Fun().

print(Node = #node{ type = node }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "n: ~p ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, Env#env.node_stats#node_stats.total_reductions}, Env#env.body);
print(Node = #node{ type = application }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "a: ~p ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, total_reductions(Node)}, Env#env.body);
print(Node = #node{ type = supervisor }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "s: ~p ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, total_reductions(Node)}, Env#env.body);
print(Node = #node{ type = worker }, Env) ->
    f(Env#env.x, Env#env.y - Env#env.shift_y, "w: ~p ", [Node#node.name], {Env#env.node_stats#node_stats.total_reductions, total_reductions(Node)}, Env#env.body).

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
            color(Window, ?WHITE);
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

rank_fraction_half_cdf(Values) ->
    Sum = lists:sum(Values),
    case Sum of
        0 ->
            "-";
        _ ->
            ValuesSort = lists:reverse(lists:sort(Values)),
            Rank50 = first_exceed(ValuesSort, Sum / 2, 0, 0),
            Rank50 / length(Values) * 2.0
    end.

first_exceed([Next | Rest], Threshold, Sum, Rank) ->
    case Next + Sum >= Threshold of
        true ->
            Rank;
        false ->
            first_exceed(Rest, Threshold, Next + Sum, Rank + 1)
    end.

color(Window, Color) ->
    color(Window, Color, false).

color(Window, Color, false) ->
    cecho:attron(Window, ?ceA_NORMAL bor ?ceCOLOR_PAIR(Color));
color(Window, Color, true) ->
    cecho:attron(Window, ?ceA_BOLD bor ?ceCOLOR_PAIR(Color)).


load_color(Fract, Window) ->
    Color =
    case Fract of
        _ when Fract < 0.333 -> ?WHITE;
        _ when Fract < 0.666 -> ?YELLOW;
        _ -> ?RED
    end,
    color(Window, Color).

%%%%%%%%%%%%%%%%%%%%%%% capturing data

capture(Node) ->
    lager:info("Capturing from ~p\n", [Node]),
    case net_adm:ping(Node) of
        pang ->
            io:format("can't connect to node, is the node up and are you using the right cookie?\n", []);
        pong ->
            Apps = [App || {App, _, _} <- rpc:call(Node, application, which_applications, [])],
            Children = [application_proc_info_request(Node, App) || App <- Apps],
            Skeleton =
            #node{type = node,
                  name = Node,
                  children = lists:filter(fun(E) -> E =/= undefined end, Children)
            },
            Tree = proc_info_collect(Skeleton),
            #capture{ tree = Tree, time = os:timestamp() }
    end.

application_proc_info_request(Node, App) ->
    case app_pid(Node, App) of
        undefined ->
            undefined;
        AppPid ->
            {AppSupPid, _}  = application_master:get_child(AppPid),
            Children        = [node(Node, Process) || Process <- rpc:call(Node, supervisor, which_children, [AppSupPid])],
            ChildrenSort    = lists:sort(fun(A, B) -> A#node.name =< B#node.name end, Children),
            #node{type      = application,
                  name      = App,
                  proc_info = proc_info_request(Node, AppPid),
                  children  = ChildrenSort
            }
    end.

app_pid(Node, App) ->
    proplists:get_value(App, proplists:get_value(running, rpc:call(Node, application_controller, info, []))).

node(Node, {Name, Pid, worker, _Mods}) ->
    #node{type      = worker,
          name      = Name,
          proc_info = proc_info_request(Node, Pid),
          children  = []
    };
node(Node, {Name, Pid, supervisor, _Mods}) ->
    #node{type      = supervisor,
          name      = Name,
          proc_info = proc_info_request(Node, Pid),
          children  = [node(Node, Process) || Process <- rpc:call(Node, supervisor, which_children, [Pid])]
    }.

proc_info_request(Node, Pid) when is_pid(Pid) ->
    Ref = make_ref(),
    spawn_link(?MODULE, proc_info_async, [Node, Pid, self(), Ref]),
    Ref.

proc_info_async(Node, Pid, From, Ref) ->
    ProcInfo1 = rpc:call(Node, erlang, process_info, [Pid]),
    Red1      = proplists:get_value(reductions, ProcInfo1),
    timer:sleep(1000),
    ProcInfo2 = rpc:call(Node, erlang, process_info, [Pid]),
    Red2      = proplists:get_value(reductions, ProcInfo2),
    Red1s     = Red2 - Red1,
    ProcInfo3 = [{reductions, Red1s} | proplists:delete(reductions, ProcInfo1)],
    ProcInfo4 = [{pid, Pid} | ProcInfo3],
    From ! {proc_info, Ref, ProcInfo4}.

proc_info_collect(Node = #node{ proc_info = undefined, children = Children }) ->
    Node#node{ children = [proc_info_collect(Child) || Child <- Children] };

proc_info_collect(Node = #node{ proc_info = Ref, children = Children }) ->
    ProcInfo =
    receive
        {proc_info, Ref, PI} ->
            PI
    after 10000 ->
        throw(timeout)
    end,
    Node#node{ proc_info = ProcInfo, children = [proc_info_collect(Child) || Child <- Children] }.

