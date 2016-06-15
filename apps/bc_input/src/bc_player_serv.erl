
-module(bc_player_serv).
-behavior(gen_server).

%% api functions
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/3]).

%% state rec
-record(state, {player_sup,
				game,
				player,
				gold,
				map_graph}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcPlayerSup, BcGame, BcPlayer, MapGraph) ->
	gen_server:start_link(?MODULE, [BcPlayerSup, BcGame, BcPlayer, MapGraph], []).

%%====================================================================
%% Gen_server functions
%%====================================================================

init(BcPlayerSup, BcGame, BcPlayer, MapGraph) ->
	BcGoldFsm = start_gold_fsm(BcPlayerSup, BcGame),
	{ok, #state{player_sup = BcPlayerSup, 
				game = BcGame, 
				player = BcPlayer, 
				map_graph = MapGraph}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_gold_fsm(Sup, BcGame) ->
	{ok, BcGoldSup} = supervisor:start_child(BcPlayerSup, #{
		id => gold_sup,
		start => {bc_gold_sup, start_link, []},
		modules => [bc_gold_sup]
	}),
	{ok, BcGoldFsm} = supervisor:start_child(BcGoldSup, #{
		id => gold_fsm,
		start => {bc_gold_fsm, start_link, []},
		modules => [bc_gold_fsm]
	}),
	EventPid = bc_game:event(BcGame),
	gen_event:add_handler(EventPid, bc_gold_event, {gold_fsm, BcGoldFsm})
	BcGoldFsm.