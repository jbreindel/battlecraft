
-module(bc_gold_fsm).
-behavior(gen_fsm).

%% api functions
-export([start_link/1, accrue/1, subtract/2]).
%% gen_fsm callbacks
-export([init/1, pending/2, accruing/2]).

%% state rec
-record(state, {player,
				gold,
				accrue_time,
				accrue_gold}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(BcPlayer) ->
	gen_fsm:start_link(?MODULE, [BcPlayer], []).

accrue(BcGoldFsm) ->
	gen_fsm:send_event(BcGoldFsm, accrue).

subtract(BcGoldFsm, Cost) ->
	gen_fsm:sync_send_event(BcGoldFsm, {gold_cost, Cost}).

%%====================================================================
%% Internal functions
%%====================================================================

init([BcPlayer]) ->
	GoldConfigFile = filename:join([code:priv_dir(bc_entity), "gold.config"]),
	{ok, [{gold, Gold}, 
		  {accrue_time, AccrueTime}, 
		  {accrue_gold, AccrueGold}]} = file:consult(GoldConfigFile),
	{ok, pending, #state{player = BcPlayer, 
						 gold = Gold, 
						 accrue_time = AccrueTime, 
						 accrue_gold = AccrueGold}}.

pending(accrue, State) ->
	gen_fsm:send_event_after(State#state.accrue_time, {gold_accrued}),
	{next_state, accruing, State}.

accruing({gold_accrued}, #state{player = BcPlayer} = State) ->
	gen_fsm:send_event_after(State#state.accrue_time, {gold_accrued}),
	PlayerPid = bc_player:pid(BcPlayer),
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
	
