-module(erl_prism_capture).
-behaviour(gen_server).

-include("include/erl_prism.hrl").
-export([equivalents/1, capture/0, prev_capture/0, next_capture/0, proc_info_async/4]).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {node, captures, capture_count, current_index}).

%% Public API

start_link(Node) ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, Node, []).

capture() ->
    gen_server:call(?MODULE, {capture}).

next_capture() ->
    gen_server:call(?MODULE, {next_capture}).

prev_capture() ->
    gen_server:call(?MODULE, {prev_capture}).

equivalents(Node) ->
    gen_server:call(?MODULE, {equivalents, Node}).

%% Callbacks

init(Node) ->
    {ok, #state{ node = Node, captures = [], capture_count = 0, current_index = 0}}.

handle_call({capture}, _From, State) ->
    Capture = capture(State#state.node),
    CurrentCount = State#state.capture_count + 1,
    CaptureIndex = set_index_and_count(Capture, CurrentCount, CurrentCount),
    {reply, CaptureIndex, State#state{ captures = State#state.captures ++ [Capture], current_index = CurrentCount, capture_count = CurrentCount}};

handle_call({next_capture}, From, State) ->
    handle_call({get_capture, State#state.current_index + 1}, From, State);

handle_call({prev_capture}, From, State) ->
    handle_call({get_capture, State#state.current_index - 1}, From, State);

handle_call({get_capture, Index}, _From, State) ->
    SaveIndex = save_index(Index, State),
    Capture  = lists:nth(SaveIndex, State#state.captures),
    CaptureIndex = set_index_and_count(Capture, SaveIndex, State#state.capture_count),
    {reply, CaptureIndex, State#state{ current_index = SaveIndex }};

handle_call({equivalents, Node}, _From, State) ->
    Equivalents = [{Capture#capture.time, hd(equivalent(Node, Capture#capture.tree))} || Capture <- State#state.captures],
    {reply, Equivalents, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.


handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

save_index(Index, State) ->
    CurrentIndex = State#state.capture_count,
    case Index of
        _ when Index < 1 ->
            1;
        _ when Index > CurrentIndex ->
           CurrentIndex;
        _ ->
            Index
    end.


%%%%%%%%%%%%%%%%%%%%%%% internal

equivalent(TemplateNode, CaptureNode) ->
    case is_equivalent(equivalents_signature(TemplateNode), equivalents_signature(CaptureNode)) of
        true ->
            CaptureNode;
        false ->
            lists:flatten([equivalent(TemplateNode, Child) || Child <- CaptureNode#node.children])
    end.

is_equivalent({pool, Name, _, _}, {pool, Name, _, _}) ->
    true;
is_equivalent({pool, _, _, _}, {_, _, _, _}) ->
    false;
is_equivalent({_, _, Pid, _}, {_, _, Pid, _}) ->
    true;
is_equivalent({_, _, _, RegisteredName}, {_, _, _, RegisteredName}) ->
    true;
is_equivalent(_, _) ->
    false.

equivalents_signature(#node{ type = Type, name = Name, proc_info = undefined }) ->
    {Type, Name, undefined, undefined};
equivalents_signature(#node{ type = Type, name = Name, proc_info = ProcInfo }) ->
    {Type, Name, proplists:get_value(pid, ProcInfo), proplists:get_value(registered_name, ProcInfo)}.

%%%%%%%%%%%%%%%%%%%%%%% capturing data

set_index_and_count(Capture, Index, Count) ->
    Capture#capture{ capture_index = Index, capture_count = Count }.

capture(Node) ->
    lager:info("Capturing from ~p\n", [Node]),
    case net_adm:ping(Node) of
        pang ->
            lager:info("can't connect to node, is the node up and are you using the right cookie?\n", []);
        pong ->
            Pids             = rpc:call(Node, erlang, processes, []),
            ProcInfoRefs     = [proc_info_request(Node, Pid) || Pid <- Pids],
            ProcInfos        = [{Pid, proc_info_collect(Ref)} || {Pid, Ref} <- ProcInfoRefs],

            Apps             = [App || {App, _, _} <- rpc:call(Node, application, which_applications, [])],
            NodeChildren     = lists:filter(fun(E) -> E =/= undefined end, [application_proc_info_request(Node, App, ProcInfos) || App <- Apps]),
            ProcsTree        = collect_pids(#node{type = node, name = Node, children = NodeChildren }),
            ProcsNonTree     = Pids -- ProcsTree,
            ProcInfosNonTree = lists:filter(fun(E) -> E =/= undefined end, [proplists:get_value(Pid, ProcInfos) || Pid <- ProcsNonTree]),
            NodesNonTree     = [#node{ type = process, totals = #totals{}, name = name(undefined, PI), proc_info = PI } || PI <- ProcInfosNonTree],
            NodesNonTreeSort = lists:sort(
                                fun(#node{name=undefined}, #node{name=undefined}) -> true; (#node{name=undefined}, _) -> false; (_, #node{name=undefined}) -> true; (A, B) -> A =< B end,
                                NodesNonTree),
            NodeChildren2    = NodeChildren ++ NodesNonTreeSort,
            Tree1            = #node{type = node, totals = #totals{}, name = Node, children = NodeChildren2 },
            Tree2            = add_totals(Tree1, reductions),
            Tree3            = add_totals(Tree2, memory),
            Tree4            = add_totals(Tree3, message_queue_len),
            lager:info("ProcsTree:~p ProcsNonTree:~p\n", [length(ProcsTree), length(ProcInfosNonTree)]),
            #capture{ process_count = length(Pids), tree = Tree4, time = os:timestamp(), totals = Tree4#node.totals }
    end.

add_totals(Node, Type) ->
    {NewNodes, _} = add_totals2(Node, Type),
    NewNodes.
add_totals2(Node, Type) ->
    Own =
    case Node#node.proc_info of
        undefined -> 0;
        ProcInfo  -> proplists:get_value(Type, ProcInfo)
    end,
    NewPairs = [add_totals2(Child, Type) || Child <- Node#node.children],
    {NewChildren, ChildValues}  = pivot(NewPairs),
    Values = lists:flatten([Own | ChildValues]),
    Sum = lists:sum(Values),
    Totals    = Node#node.totals,
    NewTotals =
    case Type of
        reductions ->
            Totals#totals{ reductions = Sum };
        memory ->
            Totals#totals{ memory = Sum };
        message_queue_len ->
            Totals#totals{ message_queue_len = Sum }
    end,
    NewNode = Node#node{ totals = NewTotals, children = lists:reverse(NewChildren) },
    {NewNode, Sum}.

pivot(Pairs) ->
    pivot(Pairs, {[], []}).

pivot([], Acc) ->
    Acc;
pivot([{Child, Value} | Rest], {ChildAcc, ValueAcc}) ->
    pivot(Rest, {[Child | ChildAcc], [Value | ValueAcc]}).

collect_pids(Tree) ->
    collect_pids(Tree, []).
collect_pids(#node{ proc_info = undefined, children = Children }, Pids) ->
    lists:foldl(fun(Child, Ps) -> collect_pids(Child, Ps) end, Pids, Children);
collect_pids(#node{ proc_info = ProcInfo, children = Children }, Pids) ->
    Pid = proplists:get_value(pid, ProcInfo),
    [Pid | lists:foldl(fun(Child, Ps) -> collect_pids(Child, Ps) end, Pids, Children)].

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
                  totals    = #totals{},
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
          totals    = #totals{},
          children  = []
    };
node(Node, {Name, Pid, supervisor, _Mods}, ProcInfos) ->
    ProcInfo = proplists:get_value(Pid, ProcInfos),
    #node{type      = supervisor,
          name      = name(Name, ProcInfo),
          proc_info = ProcInfo,
          totals    = #totals{},
          children  = maybe_pool([node(Node, Process, ProcInfos) || Process <- rpc:call(Node, supervisor, which_children, [Pid])], Name)
    }.

maybe_pool([], _) ->
    [];
maybe_pool([Node], _) ->
    [Node];
maybe_pool(Members, SupervisorName) ->
    AllWorkers = lists:all(fun(M) -> M#node.type == worker end, Members),
    InitialCalls =
    [proplists:get_value('$initial_call', proplists:get_value(dictionary, Member#node.proc_info))
     || Member <- Members],
    AllSameCalls = length(lists:usort(InitialCalls)) == 1,
    case AllWorkers and AllSameCalls of
        false  ->
            Members;
        true ->
            [
                #node{
                    name     = atom_to_list(SupervisorName) ++ "_pool",
                    type     = pool,
                    children = Members,
                    totals   = #totals{}
                }
            ]
    end.

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
    case proplists:get_value('$initial_call', proplists:get_value(dictionary, ProcInfo)) of
        {Mod, Fun, _} ->
            {ModC, FunC, _} = proplists:get_value(current_function, ProcInfo),
            iolist_to_binary(io_lib:format("~p:~p@~p:~p", [Mod, Fun, ModC, FunC]));
        undefined ->
            {Mod, Fun, Arity} = proplists:get_value(current_function, ProcInfo),
            io_lib:format("@~p:~p/~p", [Mod, Fun, Arity])
    end;
name(Name, undefined) ->
    Name;
name(Name, ProcInfo) ->
    {ModC, FunC, _} = proplists:get_value(current_function, ProcInfo),
    iolist_to_binary(io_lib:format("~p@~p:~p", [Name, ModC, FunC])).

