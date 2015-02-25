-module(eappstat_capture).

-include("include/eappstat.hrl").

-export([capture/1, proc_info_async/4]).

%%%%%%%%%%%%%%%%%%%%%%% capturing data

capture(Node) ->
    lager:info("Capturing from ~p\n", [Node]),
    case net_adm:ping(Node) of
        pang ->
            lager:info("can't connect to node, is the node up and are you using the right cookie?\n", []);
        pong ->
            Pids         = rpc:call(Node, erlang, processes, []),
            ProcInfoRefs = [proc_info_request(Node, Pid) || Pid <- Pids],
            ProcInfos    = [{Pid, proc_info_collect(Ref)} || {Pid, Ref} <- ProcInfoRefs],

            Apps = [App || {App, _, _} <- rpc:call(Node, application, which_applications, [])],
            Children = [application_proc_info_request(Node, App, ProcInfos) || App <- Apps],
            Tree =
            #node{type = node,
                  name = Node,
                  children = lists:filter(fun(E) -> E =/= undefined end, Children)
            },
            #capture{ tree = Tree, time = os:timestamp() }
    end.

proc_info_collect(Ref) ->
    ProcInfo =
    receive
        {proc_info, Ref, PI} ->
            PI
    after 10000 ->
        throw(timeout)
    end,
    ProcInfo.


application_proc_info_request(Node, App, ProcInfos) ->
    case app_pid(Node, App) of
        undefined ->
            undefined;
        AppPid ->
            {AppSupPid, _}  = application_master:get_child(AppPid),
            Children        = [node(Node, Process, ProcInfos) || Process <- rpc:call(Node, supervisor, which_children, [AppSupPid])],
            ChildrenSort    = lists:sort(fun(A, B) -> A#node.name =< B#node.name end, Children),
            #node{type      = application,
                  name      = App,
                  proc_info = proplists:get_value(AppPid, ProcInfos),
                  children  = ChildrenSort
            }
    end.

app_pid(Node, App) ->
    proplists:get_value(App, proplists:get_value(running, rpc:call(Node, application_controller, info, []))).

node(_, {Name, Pid, worker, _Mods}, ProcInfos) ->
    ProcInfo = proplists:get_value(Pid, ProcInfos),
    #node{type      = worker,
          name      = name(Name, ProcInfo),
          proc_info = ProcInfo,
          children  = []
    };
node(Node, {Name, Pid, supervisor, _Mods}, ProcInfos) ->
    ProcInfo = proplists:get_value(Pid, ProcInfos),
    #node{type      = supervisor,
          name      = name(Name, ProcInfo),
          proc_info = ProcInfo,
          children  = [node(Node, Process, ProcInfos) || Process <- rpc:call(Node, supervisor, which_children, [Pid])]
    }.

proc_info_request(Node, Pid) when is_pid(Pid) ->
    Ref = make_ref(),
    spawn_link(?MODULE, proc_info_async, [Node, Pid, self(), Ref]),
    {Pid, Ref}.

proc_info_async(Node, Pid, From, Ref) ->
    Fields    = [status, dictionary, reductions, current_function, heap_size, initial_call, memory, message_queue_len, registered_name],
    case rpc:call(Node, erlang, process_info, [Pid, reductions]) of
        undefined ->
            From ! {proc_info, Ref, undefined};
        {reductions, Red1} ->
            timer:sleep(250),
            case rpc:call(Node, erlang, process_info, [Pid, Fields]) of
                undefined ->
                    From ! {proc_info, Ref, undefined};
                ProcInfo2 ->
                    Red2      = proplists:get_value(reductions, ProcInfo2),
                    RedPerS     = (Red2 - Red1) * 4,
                    ProcInfo3 = [{reductions, RedPerS} | proplists:delete(reductions, ProcInfo2)],
                    ProcInfo4 = [{pid, Pid} | ProcInfo3],
                    From ! {proc_info, Ref, ProcInfo4}
            end
    end.

name(undefined, undefined) ->
    undefined;
name(undefined, ProcInfo) ->
    {Mod, Fun, Arity} = proplists:get_value('$initial_call', proplists:get_value(dictionary, ProcInfo)),
    iolist_to_binary(io_lib:format("~p:~p/~p", [Mod, Fun, Arity]));
name(Name, _) ->
    Name.


