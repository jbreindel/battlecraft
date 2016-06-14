
-module(bc_gold_fsm).
-behavior(gen_fsm).

%% exported funcs
-export([start_link/2, gold_cost/1]).
-export([init/2, pending/2, accrue/2]).

%% state rec
-record(state, {player_pid,
				gold,
				accrue_time,
				accrue_gold}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(PlayerPid) ->
	gen_fsm:start_link(?MODULE, [PlayerPid], []).

accrue(BcGoldFsm) ->
	gen_fsm:send_event(BcGoldFsm, accrue).

subtract(BcGoldFsm, Cost) ->
	gen_fsm:sync_send_event(BcGoldFsm, {gold_cost, Cost}).

%%====================================================================
%% Internal functions
%%====================================================================

init([PlayerPid]) ->
	GoldConfigFile = filename:join([code:priv_dir(bc_entity), "gold.config"]),
	{ok, [{gold, Gold}, 
		  {accrue_time, AccrueTime}, 
		  {accrue_gold, AccrueGold}]} = file:consult(GoldConfigFile),
	{ok, pending, #state{player_pid = PlayerPid}}.

pending(accrue, State) ->
	gen_fsm:send_event_after(State#state.accrue_time, {gold_accrued}),
	{next_state, accruing, State}.

accruing({gold_accrued}, State) ->
	gen_fsm:send_event_after(State#state.accrue_time, {gold_accrued}),
	Gold = State#state.gold + State#state.accrue_gold,
	PlayerPid ! #{event => #{type => gold_accrued, gold => Gold}},
	{next_state, accruing, State#state{gold = Gold}};
accruing({gold_cost, Cost}, State) ->
	case State#state.gold - Cost of
		Gold when Gold >= 0 ->
			{reply, {ok, Gold}, accruing, State#state{gold = Gold}};
		_ ->
			{reply, {error, not_enough_gold}, accruing, State}
	end.
	
