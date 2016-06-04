
-module(bc_game_event).
-behavior(gen_event).

-export([init/1]).

-record(state, {
				player_pid
  				}).

%%====================================================================
%% Public functions
%%====================================================================

init(#{event_manager := EventManager, 
	   player_pid := PlayerPid}) ->
	{ok, #state{player_pid = PlayerPid}}.

%% handle_event({player_join, PlayerId, Handle}, 
%% 			 #state{player_pid = PlayerPid} = State) ->
%% 	PlayerPid ! #{ event => #{type => player_join,
%% 							  player_id => PlayerId,
%% 							  handle => Handle}},
	

