
-module(bc_game_event).
-behavior(gen_event).

-export([init/1, handle_event/2]).

-record(state, {
				player_pid
  				}).

%%====================================================================
%% Public functions
%%====================================================================

init({player_pid, PlayerPid}) ->
	{ok, #state{player_pid = PlayerPid}}.

handle_event({game_started, Players}, 
			 #state{player_pid = PlayerPid} = State) ->
	PlayerPid ! {json, #{event => #{type => game_started,
									players => Players}}},
	{ok, State};
handle_event({game_error, Reason},
			 #state{player_pid = PlayerPid} = State) ->
	PlayerPid ! {json, #{event => #{type => game_error,
									reason => Reason}}},
	{ok, State};
handle_event({PlayerEventType, PlayerId, Handle}, 
			 #state{player_pid = PlayerPid} = State) ->
	PlayerPid ! {json, #{event => #{type => PlayerEventType,
									player_id => PlayerId,
									handle => Handle}}},
	{ok, State}.
