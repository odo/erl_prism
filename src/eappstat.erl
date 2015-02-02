-module(eappstat).

-export([plot/1, proc_info_and_reply/3, rank_fraction_half_cdf/1]).

plot(Node) ->
    io:format("Node: ~p @ ~p UTC\n\n", [Node, calendar:now_to_universal_time(os:timestamp())]),
    case net_adm:ping(Node) of
        pang ->
            io:format("can't connect to node, is the node up and are you using the right cookie?", []);
        pong ->
            Apps = [App || {App, _, _} <- rpc:call(Node, application, which_applications, [])],
            ProcInfoByApp    = [app_proc_info(Node, App) || App <- Apps],
            AllProcInfos = lists:append([[ProcInfo || {_Pid, ProcInfo} <- PidProcInfos] || {_App, PidProcInfos}<- ProcInfoByApp]),
            AllRedsSum = lists:sum([from_proc_info(reductions, ProcInfo) || ProcInfo <- AllProcInfos]),
            f("Reductions/s from all apps: ~p", [AllRedsSum]),
            [plot(Node, App, {AllRedsSum, proplists:get_value(App, ProcInfoByApp)}) || App <- Apps]
    end,
    ok.

app_proc_info(Node, App) ->
    case app_pid(Node, App) of
        undefined ->
            0;
        AppPid ->
            {AppSupPid, _} = application_master:get_child(AppPid),
            AppPids = pids(AppSupPid),
            {App, proc_info(Node, AppPids)}
    end.


app_pid(Node, App) ->
    proplists:get_value(App, proplists:get_value(running, rpc:call(Node, application_controller, info, []))).

plot(Node, App, {AllRedsSum, ProcInfos}) ->
    case app_pid(Node, App) of
        undefined ->
            noop;
        AppPid ->
            io:format("\n----------------------------            ~p\n", [App]),
            AppRedsSum = lists:sum([from_proc_info(reductions, ProcInfo) || {_Pid, ProcInfo} <- ProcInfos]),
            case AppRedsSum of
                0 ->
                    f("0 Reductions/s - skipping.", []);
                _ ->
                    AppPid = app_pid(Node, App),
                    {AppSupPid, _} = application_master:get_child(AppPid),
                    f("App Reductions/s ~p", [AppRedsSum], 0, {AllRedsSum, AppRedsSum}),
                    f("----------------------------", []),
                    [plot_process(Node, Process, {AllRedsSum, ProcInfos}, 1) || Process <- supervisor:which_children(AppSupPid)]
            end
    end,
    ok.

plot_process(_Node, {Name, Pid, worker, _Mods}, Reds, Depth) ->
    f("w: ~p ", [Name], Depth, reds_for(Pid, Reds));
plot_process(Node, {Name, Pid, supervisor, _Mods}, {AllRedsSum, ProcInfos}, Depth) ->
    f("s: ~p ", [Name], Depth, reds_for(Pid, {AllRedsSum, ProcInfos})),
    Children =  rpc:call(Node, supervisor, which_children, [Pid]),
    case is_pool(Children, ProcInfos) of
        true ->
            plot_pool(Node, Children, {AllRedsSum, ProcInfos}, Depth);
        false ->
            [plot_process(Node, Process, {AllRedsSum, ProcInfos}, Depth + 1) || Process <- Children]
    end.

reds_for(Pid, {AllRedsSum, ProcInfos}) ->
     {AllRedsSum, from_proc_info(reductions, proplists:get_value(Pid, ProcInfos))}.

plot_pool(_, Children, {AllRedsSum, ProcInfos}, Depth) ->
    RedForPid = fun(Pid) ->
        {_, ProcReds} = reds_for(Pid, {AllRedsSum, ProcInfos}),
        ProcReds
    end,
    Pids = [Pid || {_, Pid, _, _} <- Children],
    Reductions = [RedForPid(Pid) || Pid <- Pids],
    RedsSum = lists:sum(Reductions),
    case RedsSum of
        0 ->
            f("p: ~p x", [length(Children)], Depth + 1, {AllRedsSum, RedsSum});
        _ ->
            LoadUniformity = rank_fraction_half_cdf(Reductions) * 100,
            f("p: ~p x, Load uniformity ~.1f%", [length(Children), LoadUniformity], Depth + 1, {AllRedsSum, RedsSum})
    end.

proc_info_async(Node, Pid) ->
    spawn(?MODULE, proc_info_and_reply, [Node, Pid, self()]).

proc_info_and_reply(Node, Pid, From) ->
    ProcInfo = proc_info(Node, Pid),
    From ! {proc_info, Pid, ProcInfo}.

proc_info(Node, Pids) when is_list(Pids) ->
    [proc_info_async(Node, Pid) || Pid <- Pids],
    wait_for_proc_info(length(Pids), []);

proc_info(Node, Process) ->
    Red1 = from_proc_info(reductions, pid_info(Node, Process)),
    timer:sleep(1000),
    ProcInfo = pid_info(Node, Process),
    Red2  = from_proc_info(reductions, ProcInfo),
    Red1s = Red2 - Red1,
    [{reductions, Red1s} | proplists:delete(reductions, ProcInfo)].

wait_for_proc_info(0, Acc) ->
    Acc;
wait_for_proc_info(Length, Acc) ->
    receive
        {proc_info, Pid, Reductions} ->
            wait_for_proc_info(Length - 1, [{Pid, Reductions} | Acc])
    after 5000 ->
        throw(timeout)
    end.

pids(AppSupPid) when is_pid(AppSupPid) ->
    lists:flatten(pids({'_', AppSupPid, supervisor, '_'}, [])).

pids({_, Pid, worker, _}, Acc) ->
    [Pid | Acc];
pids({_, Pid, supervisor, _}, Acc) ->
    [Pid | [pids(Proc, Acc) || Proc <- supervisor:which_children(Pid)]].

from_proc_info(Key, ProcInfo) ->
    proplists:get_value(Key, ProcInfo).

is_pool([_], _) ->
    false;
is_pool(Children, ProcInfos) ->
    Pids = [Pid || {_, Pid, worker, _} <- Children],
    InitalCalls = [from_proc_info(initial_call, proplists:get_value(Pid, ProcInfos)) || Pid <- Pids],
    length(lists:usort(InitalCalls)) == 1.

pid_info(Node, {_, Pid, _, _}) ->
    pid_info(Node, Pid);
pid_info(Node, Pid) ->
    rpc:call(Node, erlang, process_info, [Pid]).

f(String, Args) ->
    f(String, Args, 0).

f(String, Args, Depth) ->
    io:format(fnorm(String, Args, Depth) ++ "\n", []).

fnorm(String, Args, Depth) ->
    Spacer          = ["  " || _ <- lists:seq(1, Depth)],
    StringFormat    = binary_to_list(iolist_to_binary(io_lib:format(Spacer ++ String, Args))),
    StringFormatPad = StringFormat ++ [" "||_<-lists:seq(1, 50)],
    string:substr(StringFormatPad, 1, 40).

f(String, Args, Depth, {AppReds, ProcReds}) ->
    Left  = fnorm(String, Args, Depth),
    Fract = ProcReds / AppReds,
    Bar   = trunc(Fract * 32),
    Right = ["-" || _ <- lists:seq(1, Bar)] ++ io_lib:format("~.1f%", [Fract * 100]),
    io:format(Left ++ Right ++ "\n", []).

rank_fraction_half_cdf(Values) ->
    Sum = lists:sum(Values),
    ValuesSort = lists:reverse(lists:sort(Values)),
    Rank50 = first_exceed(ValuesSort, Sum / 2, 0, 0),
    Rank50 / length(Values) * 2.

first_exceed([Next | Rest], Threshold, Sum, Rank) ->
    case Next + Sum >= Threshold of
        true ->
            Rank;
        false ->
            first_exceed(Rest, Threshold, Next + Sum, Rank + 1)
    end.
