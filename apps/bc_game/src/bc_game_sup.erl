%%%-------------------------------------------------------------------
%% @doc bc_game_serv_sup top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(bc_game_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	supervisor:start_link(?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
	{ok, {#{strategy => one_for_all, 
			intensity => 1, 
			period => 1}, []}}.
