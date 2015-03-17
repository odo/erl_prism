-module(eappstat_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    Header = {
      eappstat_header, {
        eappstat_header,
        start_link,
        []
       },
        permanent,
        2000,
        worker,
        [eappstat_header]},
    {ok, { {one_for_one, 5, 10}, [Header]} }.



