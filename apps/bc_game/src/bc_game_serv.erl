
-module(bc_game_serv).
-behavior(gen_server).

%% api functions
-export([start_link/1,
		 get_player/1,
		 get_all_players/1,
		 join/2,
		 quit/2
		]).

%% gen_server callbacks
-export([init/1,
		handle_call/3,
		handle_cast/2
		]).

%% state rec
-record(state, {
				sup,
				players,
				state_event_manager
				}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcGameSup) ->
	gen_server:start_link(?MODULE, [BcGameSup], []).

get_player(GamePid, PlayerId) ->
	gen_server:call(GamePid, {player, PlayerId}).

get_all_players(GamePid) ->
	gen_server:call(GamePid, all_players).

join(GamePid, Handle) ->
	gen_server:call(GamePid, {join, PlayerPid, Handle}).

quit(GamePid, PlayerId) ->
	gen_server:call(GamePid, {quit, PlayerId}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init(BcGameSup) ->
	{ok, #state{sup = BcGameSup, players = dict:new()}}.
	
%% handle_call({create_game, Privacy}, _From, State}) ->
%% 	case new_game(Privacy) of
%% 		{ok, GameId} ->
%% 			bc_game_sup:start_child(bc_game_fsm, )

%%====================================================================
%% Internal functions
%%====================================================================



