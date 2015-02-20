-module(eappstat).
-include("cecho.hrl").

-export([start/1, capture_and_plot/2, capture/1, plot/2, proc_info_async/4]).

-record(capture, {tree, time}).
-record(node, {type, name, proc_info, pid, children = []}).
-record(env, {time, total_reductions, x, y, x_max, y_max, cursor_y, toggle_open, open_pids = sets:new()}).

-define(WHITE, 1).
-define(GREEN, 2).
-define(YELLOW, 3).
-define(RED, 4).
-define(WHITE_HL, 5).
-define(GREEN_HL, 6).
-define(YELLOW_HL, 7).
-define(RED_HL, 8).


start(Node) ->
    setup(),
    input(Node).

input(Node) ->
    Env = #env{ cursor_y = 0},
    Capture = capture(Node),
    plot(Capture, Env),
    input(Node, Capture, Env).

input(Node, Capture, Env) ->
    {CaptureNew, EnvNew} =
    case [cecho:getch()] of
        [10] ->
            % capture
            {capture_and_plot(Node, Env), Env};
         "f" ->
            % down
            EnvDown = Env#env{ cursor_y = Env#env.cursor_y + 1 },
            EnvPlot = plot(Capture, EnvDown),
            {Capture, EnvPlot};
         "r" ->
            % up
            EnvDown = Env#env{ cursor_y = Env#env.cursor_y - 1 },
            EnvPlot = plot(Capture, EnvDown),
            {Capture, EnvPlot};
         " " ->
            % we toggle during plotting
            EnvPlot = plot(Capture, Env#env{ toggle_open = true }),
            {Capture, EnvPlot};
        Else ->
            io:format("~p", Else),
            {Capture, Env}
    end,
    input(Node, CaptureNew, EnvNew).

setup() ->
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
    cecho:init_pair(?RED_HL, ?ceCOLOR_RED, ?ceCOLOR_BLUE).

capture_and_plot(Node, Env) ->
    Capture = capture(Node),
    plot(Capture, Env),
    Capture.

%%%%%%%%%%%%%%%%%%%%%%% capturing data

capture(Node) ->
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
    ProcInfo  = [{reductions, Red1s} | proplists:delete(reductions, ProcInfo1)],
    From ! {proc_info, Ref, ProcInfo}.

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

%%%%%%%%%%%%%%%%%%%%%%% plotting

plot(Capture, Env) ->
    cecho:erase(),
    Res = do_plot(Capture, Env),
    cecho:refresh(),
    Res.

do_plot(#capture{ tree = Tree, time = Time }, Env) ->
    ReductionSum = reductions_sum(Tree),
    EnvNew = Env#env{
     total_reductions = ReductionSum,
     x = 0,
     y = 2
    },
    f(0, 0, "limits ~p", [cecho:getmaxyx()]),
    f(0, 0, "captured at ~p UTC", [calendar:now_to_universal_time(Time)]),
    f(0, 1, "total reductions/s: ~p", [ReductionSum]),
    plot_table(Tree, EnvNew).

reductions_sum(#node{ proc_info = ProcInfo, children = Children }) ->
    Increment =
    case ProcInfo of
        undefined -> 0;
        _         -> proplists:get_value(reductions, ProcInfo)
    end,
    Increment + lists:sum([reductions_sum(Child) || Child <- Children]).

plot_table(Node, Env) ->
    with_color(fun() -> print(Node, Env) end, Env),
    NewEnv = Env#env{ y = Env#env.y + 1, x = Env#env.x + 1 },
    case is_pool(Node#node.children) of
        true ->
            PoolEnv = plot_pool(Node#node.children, NewEnv#env{ x = NewEnv#env.x }),
            PoolEnv#env{ x = NewEnv#env.x - 1 };
        false ->
            ChildEnv = lists:foldl(fun(Child, FoldEnv) -> plot_table(Child, FoldEnv) end, NewEnv, Node#node.children),
            ChildEnv#env{ x = NewEnv#env.x - 1}
    end.

plot_pool(Members, Env) ->
    Reductions = [reductions_sum(Member) || Member <- Members],
    Balance    = rank_fraction_half_cdf(Reductions),
    case is_number(Balance) of
        true ->
            with_color(
                fun() -> f(Env#env.x, Env#env.y, "p: procs: ~p balance: ~.1f %", [length(Members), Balance * 100], Env#env.x + 1) end,
                Env
             );
        false ->
            with_color(
                fun() -> f(Env#env.x, Env#env.y, "p: procs: ~p balance: ~s", [length(Members), Balance], Env#env.x + 1) end,
                Env
            )
    end,
    Env#env{ y = Env#env.y + 1 }.

with_color(Fun, #env{ cursor_y = CursorY, y = CursorY }) ->
    color(?WHITE_HL),
    Fun(),
    color(?WHITE);
with_color(Fun, _) ->
    Fun().

print(Node = #node{ type = node }, Env) ->
    f(Env#env.x, Env#env.y, "n: ~p", [Node#node.name], Env#env.x,  {Env#env.total_reductions, Env#env.total_reductions});
print(Node = #node{ type = application }, Env) ->
    f(Env#env.x, Env#env.y, "a: ~p ", [Node#node.name], Env#env.x, {Env#env.total_reductions, reductions_sum(Node)});
print(Node = #node{ type = supervisor }, Env) ->
    f(Env#env.x, Env#env.y, "s: ~p ", [Node#node.name], Env#env.x, {Env#env.total_reductions, reductions_sum(Node)});
print(Node = #node{ type = worker }, Env) ->
    f(Env#env.x, Env#env.y, "w: ~p ", [Node#node.name], Env#env.x, {Env#env.total_reductions, reductions_sum(Node)});
print(_, _) ->
    noop.

is_pool([_]) ->
    false;
is_pool(Members) ->
    AllWorkers = lists:all(fun(M) -> M#node.type == worker end, Members),
    InitialCalls =
    [proplists:get_value('$initial_call', proplists:get_value(dictionary, Member#node.proc_info))
     || Member <- Members],
    AllSameCalls = length(lists:usort(InitialCalls)) == 1,
    AllWorkers and AllSameCalls.

f(X, Y, String, Args) ->
    f(X, Y, String, Args, 0).

f(X, Y, String, Args, Depth) ->
    case move_if_ok(Y, X) of
        ok ->
            cecho:addstr(io_lib:format(fnorm(String, Args) ++ "\n", []));
        not_ok ->
            noop
    end.

f(X, Y, String, Args, Depth, {AppReds, ProcReds}) ->
    Left  = fnorm(String, Args),
    Fract = ProcReds / AppReds,
    Bar   = trunc(Fract * 32),
    Right = ["-" || _ <- lists:seq(1, Bar)] ++ io_lib:format("~.1f%", [Fract * 100]),
    case move_if_ok(Y, X) of
        ok ->
            cecho:addstr(Left),
            cecho:move(Y, 50),
            load_color(Fract),
            cecho:addstr(Right),
            color(?WHITE);
        not_ok ->
            noop
    end.

move_if_ok(X, Y) ->
    {My, _Mx} = cecho:getmaxyx(),
    case Y < My of
        true ->
            cecho:move(X, Y),
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

color(Color) ->
    cecho:attron(?ceCOLOR_PAIR(Color)).

load_color(Fract) ->
    Color =
    case Fract of
        _ when Fract < 0.333 -> ?WHITE;
        _ when Fract < 0.666 -> ?YELLOW;
        _ -> ?RED
    end,
    color(Color).
