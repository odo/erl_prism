-module(erl_prism_sup).

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
        erl_prism_capture, { erl_prism_capture, start_link, [Node] },
        permanent, 2000, worker, [erl_prism_capture]
     },
    Header = {
        erl_prism_header, { erl_prism_header, start_link, [] },
        permanent, 2000, worker, [erl_prism_header]
     },
    Footer = {
        erl_prism_footer, { erl_prism_footer, start_link, [] },
        permanent, 2000, worker, [erl_prism_footer]
     },
    {ok, { {one_for_one, 5, 10}, [Capture, Header, Footer]} }.



