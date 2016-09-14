%%%-------------------------------------------------------------------
%% @doc bc_manager_sup top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bc_manager_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, {
		#{strategy => one_for_all,
		  intensity => 0,
		  period => 1}, 
			[#{
			   id => bc_manager_serv,
			   start => {bc_manager_serv, start_link, []},
			   modules => [bc_manager_serv]
			}]
		}}.
