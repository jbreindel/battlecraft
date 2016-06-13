
-module(bc_gold_fsm).
-behavior(gen_fsm).

%% exported funcs
-export([start_link/2, gold_cost/1]).
-export([init/2, pending/2, accrue/2]).

%% state rec
-record(state, {player_id,
				player_pid,
				gold,
				accrue_time,
				accrue_gold}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(PlayerId, PlayerPid) ->
	gen_fsm:start_link(?MODULE, [PlayerId, 
								 PlayerPid], []).

init([PlayerId, PlayerPid]) ->
	%% TODO load gold configuration
	{ok, pending, #state{player_id = PlayerId,
						 player_pid = PlayerPid}}.
	
