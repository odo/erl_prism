-module(erl_prism_auto_capture).

-behaviour(gen_server).
-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% Public API

start_link(Timeout) ->
    gen_server:start_link(?MODULE, Timeout, []).

%% Callbacks

init(Timeout) ->
    {ok, [Timeout], Timeout}.

handle_call(_Request, _From, [Timeout]) ->
    {reply, ok, [Timeout], Timeout}.

handle_cast(_Msg, [Timeout]) ->
    {noreply, [Timeout], Timeout}.

handle_info(timeout, [Timeout]) ->
    erl_prism ! capture,
    {noreply, [Timeout], Timeout}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
