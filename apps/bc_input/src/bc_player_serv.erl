
-module(bc_player_serv).
-behavior(gen_server).

%% api functions
-export([start_link/6]).

%% gen_server callbacks
-export([init/1]).

%% state rec
-record(state, {entity_sup,
				game,
				player,
				base_num,
				spawn_matrix,
				gold,
				map,
				entities}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcEntitySup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities) ->
	gen_server:start_link(?MODULE, [BcEntitySup, 
									BcGame, 
									BcPlayer, 
									BcGoldFsm, 
									BcMap, 
									BcEntities], []).

spawn_entities(BcPlayerServ, EntityType) ->
	gen_server:cast(BcPlayerServ, {spawn_entities, EntityType}).

%%====================================================================
%% Gen_server functions
%%====================================================================

init([BcEntitySup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities]) ->
	{ok, #state{entity_sup = BcEntitySup,
				game = BcGame, 
				player = BcPlayer,
				base_num = undefined,
				spawn_matrix = undefined,
				gold = BcGoldFsm,
				map = BcMap,
				entities = BcEntities}}.

handle_cast({spawn_entities, EntityType}, _From, State) ->
	case bc_entities:entity_config(EntityType) of
		{ok, BcEntityConfig} ->
			{ok, BaseNum, State1} = player_num(State),
			{ok, SpawnBcVertices, State2} = spawn_matrix(State1),
			spawn_entities(EntityType, State2),
			{ok, State2};
		error ->
			{ok, State}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

base_num(BaseBcVertices, BcMap) ->
	BaseBcVertex = lists:nth(1, BaseBcVertices),
	case lists:any(fun(Base1BcVertex) -> 
					  bc_vertex:row(Base1BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso
						  bc_vertex:col(Base1BcVertex) =:= bc_vertex:col(BaseBcVertex)
				   end, bc_map:base1_vertices(BcMap)) of
		true -> 1;
		false -> 
			case lists:any(fun(Base2BcVertex) ->
						       bc_vertex:row(Base2BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso
								   bc_vertex:col(Base2BcVertex) =:= bc_vertex:col(BaseBcVertex) 
				   		   end, bc_map:base2_vertices(BcMap)) of
				true -> 2;
				false ->
					case lists:any(fun(Base3BcVertex) -> 
								       bc_vertex:row(Base3BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso 
										   bc_vertex:col(Base3BcVertex) =:= bc_vertex:col(BaseBcVertex) 
								   end, bc_map:base3_vertices(BcMap)) of
						true -> 3;
						false ->
							case lists:any(fun(Base4BcVertex) -> 
								       			bc_vertex:row(Base4BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso 
													bc_vertex:col(Base4BcVertex) =:= bc_vertex:col(BaseBcVertex) 
										   end, bc_map:base4_vertices(BcMap)) of
								true -> 4;
								false -> undefined
							end
					end
			end
	end.

player_num(#state{player = BcPlayer,
				  base_num = BaseNum,
				  map = BcMap, 
				  entities = BcEntities} = State) ->
	case BaseNum of
		undefined ->
			PlayerId = bc_player:id(BcPlayer),
			BaseBcEntities = bc_entity:query_type(base, BcEntities),
			case lists:filter(fun(BaseBcEntity) -> 
								  PlayerId =:= bc_entity:player_id(BaseBcEntity) 
							  end, BaseBcEntities) of
				[] ->
					{ok, undefined, State};
				[PlayerBaseBcEntity] ->
					Uuid = bc_entity:uuid(PlayerBaseBcEntity),
					QueryResults = bc_map:query_collisions(Uuid, BcEntities),
					BcVertices = lists:map(fun(#{vertex := BcVertex}) -> 
											  BcVertex 
										   end, QueryResults),
					Num = base_num(BcVertices, BcMap),
					{ok, Num, State#state{base_num = Num}}
			end;
		Num when is_integer(Num) ->
			{ok, Num, State}
	end.

%% do_spawn_entities(BcEntityConfig, SpawnBcVertices, BatchCount, 
%% 				  #state{entities = BcEntities,
%% 						 map = BcMap,
%% 						 base_num = BaseNum,
%% 						 spawn_matrix = BcMatrix} = State) ->
%% 	SpawnBcVertices = spawn_vertices(Offset, State),
%% 	EntitySize = bc_entity_config:size(BcEntityConfig),
%% 	BatchSize = length(SpawnBcVertices) / EntitySize,
%% 	
%% 
%% spawn_entities(BcEntityConfig, #state{entities = BcEntities,
%% 								  	  map = BcMap,
%% 									  base_num = BaseNum,
%% 								  	  spawn_matrix = BcMatrix} = State) ->
%% 	
%% 	%% TODO spawn entities
%% 	ok.

spawn_vertices(Offset, #state{base_num = BaseNum,
					  		  spawn_matrix = SpawnBcMatrix} = State) ->
	case BaseNum of
		1 -> 
			MinRow = bc_matrix:min_row(SpawnBcMatrix),
			bc_matrix:row(MinRow + Offset, SpawnBcMatrix);
		2 ->
			MaxCol = bc_matrix:max_col(SpawnBcMatrix),
			bc_matrix:col(MaxCol - Offset, SpawnBcMatrix);
		3 ->
			MaxRow = bc_matrix:max_row(SpawnBcMatrix),
			bc_matrix:row(MaxRow - Offset, SpawnBcMatrix);
		4 ->
			MinCol = bc_matrix:min_col(SpawnBcMatrix),
			bc_matrix:col(MinCol + Offset, SpawnBcMatrix)
	end.

spawn_matrix(#state{base_num = BaseNum,
					spawn_matrix = SpawnBcMatrix,
					map = BcMap} = State) ->
	case SpawnBcMatrix of
		undefined ->
			BcVertices = case BaseNum of
							 1 -> bc_map:base1_spawn_vertices(BcMap);
							 2 -> bc_map:base2_spawn_vertices(BcMap);
							 3 -> bc_map:base3_spawn_vertices(BcMap);
							 4 -> bc_map:base4_spawn_vertices(BcMap)
						 end,
			BcMatrix = bc_matrix:init(BcVertices),
			{ok, BcMatrix, State#state{spawn_matrix = BcMatrix}};
		BcMatrix ->
			{ok, BcMatrix, State}
	end.
