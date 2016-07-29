
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
				map,
				entities}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcPlayerSup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities) ->
	gen_server:start_link(?MODULE, [BcPlayerSup, 
									BcGame, 
									BcPlayer, 
									BcGoldFsm, 
									BcMap, 
									BcEntities], []).

%%====================================================================
%% Gen_server functions
%%====================================================================

init([BcPlayerSup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities]) ->
	{ok, #state{player_sup = BcPlayerSup,
				game = BcGame, 
				player = BcPlayer,
				gold = BcGoldFsm,
				map = BcMap,
				entities = BcEntities}}.
