
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
				game_id,
				players
				}).

%%====================================================================
%% API functions
%%====================================================================

start_link(GameId, BcGameSup) ->
	gen_server:start_link(?MODULE, [GameId, BcGameSup], []).

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

init(GameId, BcGameSup) ->
	self() ! {start_game_fsm, GameId, BcGameSup},
	{ok, #state{sup = BcGameSup, game_id = GameId, players = dict:new()}}.
	
handle_info({start_game_fsm, GameId, BcGameSup}, State) ->
	{ok, Pid} = supervisor:start_child(BcGameSup, 
		#{id => GameId,
			start => {bc_game_fsm, start_link, [GameId]},
			restart => temporary,
			shutdown => 10000,
			type => worker,
			modules => [bc_game_fsm]}),
	%% TODO link pid
	{noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================



