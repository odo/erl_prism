-module(eappstat_header).
-include("include/eappstat.hrl").

-behaviour(gen_server).
-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([set_env/1, set_capture/1, plot/0]).

-record(state, {env, capture}).

%% Public API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

set_env(Env) ->
    gen_server:call(?MODULE, {set_env, Env}).

set_capture(Capture) ->
    gen_server:call(?MODULE, {set_capture, Capture}).

plot() ->
    gen_server:call(?MODULE, {plot}).

%% Callbacks

init([]) ->
    {ok, #state{}}.

handle_call({set_env, Env}, _From, State) ->
    {reply, ok, State#state{ env = Env }};

handle_call({set_capture, Capture}, _From, State) ->
    {reply, ok, State#state{ capture = Capture }};

% helium@silverfish.fritz.box                   2015-3-17T20:52:38
% Reductions:  10k  Memory: 1M   Messagequeue: 23


handle_call({plot}, _From, State) ->
    #capture{ tree = Tree, time = Time } = State#state.capture,
    #env{ header = Header, node_stats = NodeStats, mode = Mode } = State#state.env,
    eappstat_utils:color(Header, ?WHITE_TYPE),
    {{Y, M, D}, {Hr, Min, Sec}} = calendar:now_to_universal_time(Time),
    f(1,  0, "~s", [Tree#node.name], Header),
    f(45, 0, "~p-~p-~pT~p:~p:~p", [Y, M, D, Hr, Min, Sec], Header),
    {RedsValue, RedsOOM} = eappstat_utils:oom(NodeStats#node_stats.reductions, 1000),
    maybe_hl(Mode, reductions, Header),
    f(1,  1, "Reductions/s: ~.1f ~s", [RedsValue, RedsOOM], Header),
    {MemValue, MemOOM} = eappstat_utils:oom(NodeStats#node_stats.memory, 1024),
    maybe_hl(Mode, memory, Header),
    f(25, 1, "Memory: ~.1f ~sB", [MemValue, MemOOM], Header),
    {QueueValue, QueueOOM} = eappstat_utils:oom(NodeStats#node_stats.message_queue_len, 1000),
    maybe_hl(Mode, message_queue_len, Header),
    f(45, 1, "messageQueue: ~.1f ~s", [QueueValue, QueueOOM], Header),
    cecho:wrefresh(Header),
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

f(X, Y, String, Args, Window) ->
    cecho:wmove(Window, Y, X),
    cecho:waddstr(Window, io_lib:format(eappstat_utils:fnorm(String, Args) ++ "\n", [])).

maybe_hl(Mode, Mode, Window) ->
    eappstat_utils:color(Window, ?WHITE_HL_TYPE);
maybe_hl(_, _, Window) ->
    eappstat_utils:color(Window, ?WHITE_TYPE).

