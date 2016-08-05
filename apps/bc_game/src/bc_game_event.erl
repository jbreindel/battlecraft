
-module(bc_game_event).
-behavior(gen_event).

-export([init/1, handle_event/2]).

%% internal state
-record(state, {player}).

%%====================================================================
%% Public functions
%%====================================================================

init({player, BcPlayer}) ->
	{ok, #state{player = BcPlayer}}.

handle_event({game_started, BcPlayers}, 
			 #state{player = BcPlayer} = State) ->
	PlayerPid = bc_player:pid(BcPlayer),
	PlayerPid ! #{type => game_event,
				  game_event => #{event_type => game_started,
							 	  players => lists:map(fun bc_player:serialize/1, BcPlayers)}},
	{ok, State};
handle_event({game_error, Reason},
			 #state{player = BcPlayer} = State) ->
	PlayerPid = bc_player:pid(BcPlayer),
	PlayerPid ! #{type => game_event,
				  game_event => #{event_type => game_error,
							 	  reason => Reason}},
	{ok, State};
handle_event({PlayerEventType, BcPlayer}, 
			 #state{player = BcPlayer} = State) ->
	PlayerPid = bc_player:pid(BcPlayer),
	PlayerPid ! #{type => game_event,
				  game_event => #{event_type => PlayerEventType,
							 	  player => bc_player:serialize(BcPlayer)}},
	{ok, State}.
