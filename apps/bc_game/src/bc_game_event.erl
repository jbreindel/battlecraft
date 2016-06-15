
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

handle_event({game_started, Players}, 
			 #state{player = BcPlayer} = State) ->
	PlayerPid = bc_player:pid(BcPlayer),
	PlayerPid ! #{event => #{type => game_started,
							 players => Players}},
	{ok, State};
handle_event({game_error, Reason},
			 #state{player = BcPlayer} = State) ->
	PlayerPid = bc_player:pid(BcPlayer),
	PlayerPid ! #{event => #{type => game_error,
							 reason => Reason}},
	{ok, State};
handle_event({PlayerEventType, BcPlayer}, 
			 #state{player = BcPlayer} = State) ->
	PlayerPid = bc_player:pid(BcPlayer),
	PlayerPid ! #{event => #{type => PlayerEventType,
							 player => BcPlayer}},
	{ok, State}.
