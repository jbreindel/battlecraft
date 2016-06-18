
-module(bc_player_serv).
-behavior(gen_server).

%% api functions
-export([start_link/5]).

%% gen_server callbacks
-export([init/1]).

%% state rec
-record(state, {player_sup,
				game,
				player,
				gold,
				map_graph}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcPlayerSup, BcGame, BcPlayer, BcGoldFsm, MapGraph) ->
	gen_server:start_link(?MODULE, [BcPlayerSup, 
									BcGame, 
									BcPlayer, 
									BcGoldFsm, 
									MapGraph], []).

%%====================================================================
%% Gen_server functions
%%====================================================================

init([BcPlayerSup, BcGame, BcPlayer, BcGoldFsm, MapGraph]) ->
	{ok, #state{player_sup = BcPlayerSup,
				game = BcGame, 
				player = BcPlayer,
				gold = BcGoldFsm,
				map_graph = MapGraph}}.
