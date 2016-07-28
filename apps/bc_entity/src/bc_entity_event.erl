
-module(bc_entity_event).
-behavior(gen_event).

-export([init/1, 
		 handle_event/2]).

%% internal state
-record(state, {player}).

%%====================================================================
%% Public functions
%%====================================================================

init([BcPlayer]) ->
	{ok, #state{player = BcPlayer}}.

handle_event({EntityEventType, BcEntity},
			 #state{player = BcPlayer} = State) ->
	PlayerPid = bc_player:pid(BcPlayer),
	PlayerPid ! #{type => entity_event,
				  entity_event => #{event_type => EntityEventType,
									entity => BcEntity}},
	{ok, State}.
	