-module(erl_prism_export).
-include("include/erl_prism.hrl").

-ifdef(TEST).
-compile([export_all]).
-endif.

-export([export/2]).

export(Capture, Env) ->
    {ok, Dir} = application:get_env(erl_prism, dir),
    TemplateNode = erl_prism_footer:node(),
    lager:error("export\n", []),
    Type = TemplateNode#node.type,
    case Type of
        node        -> noop;
        application -> noop;
        _ ->
            Captures = erl_prism_capture:equivalents(TemplateNode),
            lager:error("found ~p Captures\n", [length(Captures)]),
            lager:error("template: ~p\n", [TemplateNode]),
            lager:error("captures: ~p\n",  [Captures]),
            Mode = Env#env.mode,
            Path = Dir ++ "/" ++ file_name(TemplateNode) ++ "_" ++ atom_to_list(Mode) ++ ".cvs",
            Data = [csv_header(Captures), [csv_line(Node#node.type, Time, Node, Mode) || {Time, Node} <- Captures]],
            lager:error("csvbin:~p", [iolist_to_binary(Data)]),
            ok   = file:write_file(Path, iolist_to_binary(Data))
    end.

csv_header([{_, Node} | _ ] = Captures) ->
    csv_header(Node#node.type, Captures).
csv_header(pool, Captures) ->
    header(pool, Captures);
csv_header(Type, Captures) ->
    header(Type, Captures).

header(pool, Captures) ->
    HeaderNames = lists:append([header_names(Node#node.children) || {_, Node} <- Captures]),
    HeaderNamesUniq = lists:usort(HeaderNames),
    ["time", ",", string:join(HeaderNamesUniq, ","), "\n"];

header(_, Captures) ->
    HeaderNames = header_names(Captures),
    ["time", ",", string:join(HeaderNames, ","), "\n"].

header_names(Nodes) ->
    lists:usort([header_name(Node) || Node <- Nodes]).

header_name({_, Node}) ->
    header_name(Node);
header_name(#node{ proc_info = ProcInfo }) ->
    Name =
    case proplists:get_value(registered_name, ProcInfo) of
        [] ->
            proplists:get_value(pid, ProcInfo);
        RegName ->
            RegName
    end,
    io_lib:format("~p", [Name]).


csv_line(pool, Time, #node{ children = Children }, Mode) ->
    [time_string(Time), ",", data(pool, Children, Mode), "\n"];
csv_line(_, Time, Node, Mode) ->
    [time_string(Time), ",", data(else, Node, Mode), "\n"].

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

