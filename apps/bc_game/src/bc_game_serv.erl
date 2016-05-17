
-module(bc_game_serv).
-behavior(gen_server).

%% api functions
-export([start_link/1,
		 get_player/1,
		 get_all_players/0,
		 join/1,
		 quit/1
		]).

%% gen_server callbacks
-export(init/1,
		handle_call/3,
		handle_cast/2).

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
	gen_server:start_link({local, ?MODULE}, ?MODULE, [BcGameSup], []).

get_player(PlayerId) ->
	gen_server:call(?MODULE, {player, PlayerId}).

get_all_players() ->
	gen_server:call(?MODULE, all_players).

join(PlayerPid, Handle) ->
	gen_server:call(?MODULE, {join, PlayerPid, Handle}).

quit(PlayerId) ->
	gen_server:call(?MODULE, {quit, PlayerId}).

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



