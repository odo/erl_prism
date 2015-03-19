-module(eappstat_app).
-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, Node} = application:get_env(eappstat, node),
    eappstat_sup:start_link(Node).

stop(_State) ->
    ok.

