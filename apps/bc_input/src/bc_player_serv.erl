
-module(bc_player_serv).
-behavior(gen_server).

%% api functions
-export([start_link/6]).

%% gen_server callbacks
-export([init/1]).

%% state rec
-record(state, {player_sup,
				game,
				player,
				player_num,
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

%%====================================================================
%% Internal functions
%%====================================================================

vertex_sort(BcVertex1, BcVertex2) ->

base_num(BaseBcVertices) ->
	SortedBaseBcVertices = lists:sort(fun bc_vertex:sort/2, BaseBcVertices),
	%% TODO determine base number
	1.

player_num(#state{player = BcPlayer,
				  player_num = PlayerNum,
				  map = BcMap, 
				  entities = BcEntities} = State) ->
	PlayerId = bc_player:id(BcPlayer),
	BaseBcEntities = bc_entity:query_type(base, BcEntities),
	case lists:filter(fun(BaseBcEntity) -> 
						  PlayerId =:= bc_entity:player_id(BaseBcEntity) 
					  end, BaseBcEntities) of
		[] ->
			undefined;
		[PlayerBaseBcEntity] ->
			Uuid = bc_entity:uuid(PlayerBaseBcEntity),
			QueryResults = bc_map:query_collisions(Uuid, BcEntities),
			BcVertices = lists:map(fun(#{vertex := BcVertex}) -> BcVertex end, QueryResults),
			base_num(BcVertices)
	end.

