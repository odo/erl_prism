-module(erl_prism_footer).
-include("include/erl_prism.hrl").

-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([set_env/1, set_node/2, node/0, plot/0]).

-record(state, {env, node}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_node(Node, Env) ->
    gen_server:call(?MODULE, {set_node, Node, Env}).

set_env(Env) ->
    gen_server:call(?MODULE, {set_env, Env}).

plot() ->
    gen_server:call(?MODULE, {plot}).

node() ->
    gen_server:call(?MODULE, {node}).

%% Callbacks

init([]) ->
    {ok, #state{}}.

handle_call({set_node, Node, Env}, _From, State) ->
    {reply, ok, State#state{ node = Node, env = Env }};

handle_call({set_env, Env}, _From, State) ->
    {reply, ok, State#state{ env = Env }};

handle_call({node}, _From, State) ->
    {reply, State#state.node, State};

handle_call({plot}, _From, State = #state{ node = undefined }) ->
    {reply, ok, State};
handle_call({plot}, _From, State = #state{ node = Node = #node{ proc_info = undefined, type = Type }, env = #env{ capturing = Capturing, footer = Footer, mode = Mode } }) ->
    case Capturing of
        true ->
            erl_prism_utils:color(Footer, ?WHITE_HL_TYPE),
            erl_prism_utils:f(1,  1, "Cap", [], Footer);
        _ ->
            erl_prism_utils:f(1,  1, "   ", [], Footer)
    end,
    Members = Node#node.children,
    erl_prism_utils:color(Footer, ?WHITE_TYPE),
    Values         = [erl_prism_utils:total(Mode, Member) || Member <- Members],
    Balance        = erl_prism_utils:balance(Values),
    erl_prism_utils:f(4,  1, "                                                        ", [], Footer),
    erl_prism_utils:f(1,  2, "                                                        ", [], Footer),
    erl_prism_utils:f(1,  3, "       type: ~p", [Type], Footer),
    erl_prism_utils:f(32, 3, "Children: ~p", [length(Members)], Footer),
    case is_number(Balance) of
        true ->
            erl_prism_utils:f(52, 3, "Balance: ~.1f %", [Balance * 100], Footer);
        false ->
            erl_prism_utils:f(52, 3, "Balance: ~s", [Balance], Footer)
    end,

    ReductionsAcc  = erl_prism_utils:total(reductions, Node),
    MemoryAcc      = erl_prism_utils:total(memory, Node),
    QueueLengthAcc = erl_prism_utils:total(message_queue_len, Node),
    erl_prism_utils:color(Footer, ?WHITE_TYPE),
    erl_prism_utils:f(1,   4, "acc:  ", [], Footer),
    {RedsValueAcc, RedsOOMAcc} = erl_prism_utils:oom(ReductionsAcc, 1000),
    erl_prism_utils:maybe_hl(Mode, reductions, Footer),
    erl_prism_utils:f(7,   4, "Reductions: ~.1f ~s/s", [RedsValueAcc, RedsOOMAcc], Footer),
    {MemValueAcc, MemOOMAcc} = erl_prism_utils:oom(MemoryAcc, 1024),
    erl_prism_utils:maybe_hl(Mode, memory, Footer),
    erl_prism_utils:f(32,  4, "Memory: ~.1f ~sB", [MemValueAcc, MemOOMAcc], Footer),
    {QueueValueAcc, QueueOOMAcc} = erl_prism_utils:oom(QueueLengthAcc, 1000),
    erl_prism_utils:maybe_hl(Mode, message_queue_len, Footer),
    erl_prism_utils:f(52,  4, "messageQueue: ~.1f ~s", [QueueValueAcc, QueueOOMAcc], Footer),
    cecho:wrefresh(Footer),
    {reply, ok, State};
handle_call({plot}, _From, State = #state{ node = Node = #node{ proc_info = ProcInfo }, env = #env{ capturing = Capturing, footer = Footer, mode = Mode } }) ->
    case Capturing of
        true ->
            erl_prism_utils:color(Footer, ?WHITE_HL_TYPE),
            erl_prism_utils:f(1,  1, "Cap", [], Footer);
        _ ->
            erl_prism_utils:color(Footer, ?WHITE_TYPE),
            erl_prism_utils:f(1,  1, "   ", [], Footer)
    end,
    erl_prism_utils:color(Footer, ?WHITE_TYPE),
    Pid               = proplists:get_value(pid, ProcInfo),
    {Mod, Fun, Arity} = proplists:get_value(current_function, ProcInfo),
    RegisteredName    = proplists:get_value(registered_name, ProcInfo),
    Status            = proplists:get_value(status, ProcInfo),
    Reductions        = proplists:get_value(reductions, ProcInfo),
    Memory            = proplists:get_value(memory, ProcInfo),
    QueueLength       = proplists:get_value(message_queue_len, ProcInfo),
    ReductionsAcc     = erl_prism_utils:total(reductions, Node),
    MemoryAcc         = erl_prism_utils:total(memory, Node),
    QueueLengthAcc     = erl_prism_utils:total(message_queue_len, Node),

    % erl_prism_utils:f(1,  1, "~s with ~p is ~p at ~p:~p/~p with ~p B and ~p msgs", [Node#node.name, Pid, Status, Mod, Fun, Arity, Memory, QueueLength], Footer),

    erl_prism_utils:f(7,  1, "Pid: ~p", [Pid], Footer),
    erl_prism_utils:f(32, 1, "Registered: ~s", [RegisteredName], Footer),
    erl_prism_utils:f(7,  2, "Status: ~p", [Status], Footer),
    erl_prism_utils:f(32, 2, "CurrentFunction: ~p:~p/~p", [Mod, Fun, Arity], Footer),

    erl_prism_utils:f(1,   3, "self: ", [], Footer),
    {RedsValue, RedsOOM} = erl_prism_utils:oom(Reductions, 1000),
    erl_prism_utils:maybe_hl(Mode, reductions, Footer),
    erl_prism_utils:f(7,   3, "Reductions: ~.1f ~s/s", [RedsValue, RedsOOM], Footer),
    {MemValue, MemOOM} = erl_prism_utils:oom(Memory, 1024),
    erl_prism_utils:maybe_hl(Mode, memory, Footer),
    erl_prism_utils:f(32,  3, "Memory: ~.1f ~sB", [MemValue, MemOOM], Footer),
    {QueueValue, QueueOOM} = erl_prism_utils:oom(QueueLength, 1000),
    erl_prism_utils:maybe_hl(Mode, message_queue_len, Footer),
    erl_prism_utils:f(52,  3, "messageQueue: ~.1f ~s", [QueueValue, QueueOOM], Footer),

    erl_prism_utils:color(Footer, ?WHITE_TYPE),
    erl_prism_utils:f(1,   4, "acc:  ", [], Footer),
    {RedsValueAcc, RedsOOMAcc} = erl_prism_utils:oom(ReductionsAcc, 1000),
    erl_prism_utils:maybe_hl(Mode, reductions, Footer),
    erl_prism_utils:f(7,   4, "Reductions: ~.1f ~s/s", [RedsValueAcc, RedsOOMAcc], Footer),
    {MemValueAcc, MemOOMAcc} = erl_prism_utils:oom(MemoryAcc, 1024),
    erl_prism_utils:maybe_hl(Mode, memory, Footer),
    erl_prism_utils:f(32,  4, "Memory: ~.1f ~sB", [MemValueAcc, MemOOMAcc], Footer),
    {QueueValueAcc, QueueOOMAcc} = erl_prism_utils:oom(QueueLengthAcc, 1000),
    erl_prism_utils:maybe_hl(Mode, message_queue_len, Footer),
    erl_prism_utils:f(52,  4, "messageQueue: ~.1f ~s", [QueueValueAcc, QueueOOMAcc], Footer),

    cecho:wrefresh(Footer),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

