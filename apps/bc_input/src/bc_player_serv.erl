
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
				spawn_vertices,
				gold,
				map,
				entities}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcEntitySup, BcPlayer, BcGoldFsm, BcMap, BcEntities) ->
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
				spawn_vertices = undefined,
				gold = BcGoldFsm,
				map = BcMap,
				entities = BcEntities}}.

handle_cast({spawn_entities, EntityType}, _From, State) ->
	case bc_entities:entity_config(EntityType) of
		{ok, BcEntityConfig} ->
			{ok, BaseNum, State1} = player_num(State),
			{ok, SpawnBcVertices, State2} = spawn_vertices(State1),
			spawn_entities(EntityType, State2),
			{ok, State2};
		error ->
			{ok, State2}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

spawn_entities(BcEntityConfig, #state{entities = BcEntities,
								  map = BcMap,
								  spawn_vertices = BcVertices} = State) ->
	EntitySize = bc_entity_config:size(BcEntityConfig),
	QueryRes = bc_map:query_collisions(BcMap, BcVertices),
	%% TODO spawn entities
	ok.

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

spawn_vertices(#state{base_num = BaseNum,
					  spawn_vertices = SpawnBcVertices,
					  map = BcMap} = State) ->
	case SpawnBcVertices of
		undefined ->
			BcVertices = case BaseNum of
							 1 -> bc_map:base1_spawn_vertices(BcMap);
							 2 -> bc_map:base2_spawn_vertices(BcMap);
							 3 -> bc_map:base3_spawn_vertices(BcMap);
							 4 -> bc_map:base4_spawn_vertices(BcMap)
						 end,
			{ok, BcVertices, State#state{spawn_vertices = BcVertices}};
		BcVertices when is_list(BcVertices) 
		  			andalso length(BcVertices) > 0 ->
			{ok, SpawnBcVertices, State}
	end.
