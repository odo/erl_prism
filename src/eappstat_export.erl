-module(eappstat_export).
-include("include/eappstat.hrl").

-export([export/2]).

export(Capture, Env) ->
    Time      = Capture#capture.time,
    {ok, Dir} = application:get_env(eappstat, dir),
    Node = eappstat_footer:node(),
    Type = Node#node.type,
    case Type of
        node        -> noop;
        application -> noop;
        _ ->
            Mode = Env#env.mode,
            Path = Dir ++ "/" ++ file_name(Node) ++ "_" ++ atom_to_list(Mode) ++ ".cvs",
            Data = csv(Node#node.type, Time, Node, Mode),
            ok   = file:write_file(Path, iolist_to_binary(Data))
    end.

csv(pool, Time, #node{ children = Children }, Mode) ->
    [header(Children), time_string(Time), ",", data(pool, Children, Mode)];
csv(_, Time, Node, Mode) ->
    [header([Node]), time_string(Time), ",", data(else, Node, Mode)].

header(Children) ->
    PidString = fun(#node{ proc_info = ProcInfo }) ->
            Pid = proplists:get_value(pid, ProcInfo),
            io_lib:format("~p", [Pid])
    end,
    ["time", ",", string:join([PidString(Child) || Child <- Children], ","), "\n"].

data(pool, Children, Mode) ->
    string:join([data(worker, Child, Mode) || Child <- Children], ",");
data(_, #node{ proc_info = ProcInfo }, Mode) ->
    io_lib:format("~p", [proplists:get_value(Mode, ProcInfo)]).


file_name(#node{ type = Type, proc_info = undefined }) ->
    file_name(Type, undefined, undefined);
file_name(#node{ type = Type, proc_info = ProcInfo }) ->
    file_name(Type, proplists:get_value(registered_name, ProcInfo), ProcInfo).

file_name(pool, undefined, undefined) ->
    random_pool_name();
file_name(Type, [], ProcInfo) ->
    file_name(Type, undefined, ProcInfo);
file_name(_, undefined, ProcInfo) ->
    Pid = proplists:get_value(pid, ProcInfo),
    case proplists:get_value('$initial_call', proplists:get_value(dictionary, ProcInfo)) of
        {Mod, Fun, _} ->
            io_lib:format("~p:~p_~p", [Mod, Fun, Pid]);
        undefined ->
            [FullPid] = io_lib:format("~p", [Pid]),
            string:substr(FullPid, 2, length(FullPid) -2)
    end;
file_name(_, Name, _) ->
    atom_to_list(Name).

random_pool_name() ->
    "pool_" ++ integer_to_list(random:uniform(100000)).

time_string(Time) ->
    {{Y, M, D}, {Hr, Min, Sec}} = calendar:now_to_universal_time(Time),
    io_lib:format("~4.10.0B-~2.10.0B-~2.10.0B ~2.10.0B:~2.10.0B:~2.10.0B", [Y, M, D, Hr, Min, Sec]).

