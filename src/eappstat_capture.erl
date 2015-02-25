-module(eappstat_capture).

-include("include/eappstat.hrl").

-export([capture/1, proc_info_async/4]).

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
    Fields    = [status, dictionary, reductions, current_function, heap_size, initial_call, memory, message_queue_len, registered_name],
    {reductions, Red1} = rpc:call(Node, erlang, process_info, [Pid, reductions]),
    timer:sleep(250),
    ProcInfo2 = rpc:call(Node, erlang, process_info, [Pid, Fields]),
    Red2      = proplists:get_value(reductions, ProcInfo2),
    RedPerS     = (Red2 - Red1) * 4,
    ProcInfo3 = [{reductions, RedPerS} | proplists:delete(reductions, ProcInfo2)],
    ProcInfo4 = [{pid, Pid} | ProcInfo3],
    From ! {proc_info, Ref, ProcInfo4}.

proc_info_collect(Node = #node{ proc_info = undefined, children = Children }) ->
    Node#node{ children = [proc_info_collect(Child) || Child <- Children] };

proc_info_collect(Node = #node{ name = Name, proc_info = Ref, children = Children }) ->
    ProcInfo =
    receive
        {proc_info, Ref, PI} ->
            lager:info("proc_info: ~p\n", [PI]),
            PI
    after 10000 ->
        throw(timeout)
    end,
    NameNew = name(Name, ProcInfo),
    Node#node{ name = NameNew, proc_info = ProcInfo, children = [proc_info_collect(Child) || Child <- Children] }.

name(undefined, ProcInfo) ->
    {Mod, Fun, Arity} = proplists:get_value('$initial_call', proplists:get_value(dictionary, ProcInfo)),
    iolist_to_binary(io_lib:format("~p:~p/~p", [Mod, Fun, Arity]));
name(Name, _) ->
    Name.


