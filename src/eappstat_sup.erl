-module(eappstat_sup).

-behaviour(supervisor).

%% API
-export([start_link/1]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link(Node) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, Node).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init(Node) ->
    Capture = {
        eappstat_capture, { eappstat_capture, start_link, [Node] },
        permanent, 2000, worker, [eappstat_capture]
     },
    Header = {
        eappstat_header, { eappstat_header, start_link, [] },
        permanent, 2000, worker, [eappstat_header]
     },
    Footer = {
        eappstat_footer, { eappstat_footer, start_link, [] },
        permanent, 2000, worker, [eappstat_footer]
     },
    {ok, { {one_for_one, 5, 10}, [Capture, Header, Footer]} }.



