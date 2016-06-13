
-module(bc_money_fsm).
-behavior(gen_fsm).

%% exported funcs
-export([start_link/2, money_cost/1]).
-export([init/2, pending/2, started/2]).

%% state rec
-record(state, {player_id,
				player_pid,
				money,
				accrue_time,
				accrue_money}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(PlayerId, PlayerPid, InitialMoney, AccrueTime, AccrueMoney) ->
	gen_fsm:start_link(?MODULE, [PlayerId, 
								 PlayerPid, 
								 InitialMoney, 
								 AccrueTime, 
								 AccrueMoney], []).

init([PlayerId, PlayerPid, Money, InitialMoney, AccrueTime, AccrueMoney]) ->
	gen_fsm:send_event_after(AccrueTime, {money_accrue, AccrueMoney})
	{ok, pending, #state{player_id = PlayerId, 
						 player_pid = PlayerPid, 
						 money = Money}}.
	
