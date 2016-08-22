
-module(bc_gold_event).
-behavior(gen_event).

-export([init/1, handle_event/2]).

%% internal state
-record(state, {gold_fsm}).

%%====================================================================
%% Public functions
%%====================================================================

init({gold_fsm, BcGoldFsm}) ->
	{ok, #state{gold_fsm = BcGoldFsm}}.

handle_event({game_started, _}, 
			 #state{gold_fsm = BcGoldFsm} = State) ->
	bc_gold_fsm:accrue(BcGoldFsm),
	{ok, State};
handle_event(_, State) ->
	{ok, State}.

