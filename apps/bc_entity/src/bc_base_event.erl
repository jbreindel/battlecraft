
-module(bc_base_entities).
-behavior(gen_event).

-export([init/1, 
		 handle_event/2]).

%% internal state
-record(state, {base_uuid, 
				game, 
				player}).

%%====================================================================
%% Public functions
%%====================================================================

init([BaseUuid, BcGame, BcPlayer]) ->
	{ok, #state{base_uuid = BaseUuid, 
				game = BcGame,
				player = BcPlayer}}.

handle_event({entitiy_died, BcEntity}, #state{base_uuid = BaseUuid, 
											  game = BcGame, 
											  player = BcPlayer} = State) ->
	case BaseUuid =:= bc_entity:uuid(BcEntity) of
		true ->
			GameFsmPid = bc_game:fsm(BcGame),
			PlayerId = bc_player:id(BcPlayer),
			bc_game_fsm:player_out(GameFsmPid, PlayerId),
			remove_handler;
		false ->
			{ok, State}
	end.