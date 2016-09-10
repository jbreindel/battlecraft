
-module(bc_entity_util).

-export([spawn_entity/7,
		 entity_distance/2,
		 vertex_distance/2,
		 iolist_to_entity_type/1]).

-spec spawn_entity(BcCollision :: bc_collision:collision(), 
				   BcPlayer :: bc_player:player(), 
				   BcEntitySup :: pid(),
				   BcEntityConfig :: bc_entity_config:entity_config(), 
				   Orientation :: atom(), 
				   BcMap :: bc_map:map_graph(),
				   BcEntities :: bc_entites:entity()) -> {ok, BcEntity :: bc_entity:entity()} |
															 {error_entity, Reason :: string()} |
															 {error_collision, Reason :: string()}.
spawn_entity(BcCollision, BcPlayer, BcEntitySup, 
			 BcEntityConfig, Orientation, BcMap, BcEntities) ->
	case bc_map:insert_collision(BcMap, BcCollision) of
		true ->
			BcEntity = create_entity(BcCollision, BcPlayer, BcEntitySup, 
									 BcEntityConfig, Orientation, BcMap, BcEntities),
			case bc_entities:insert_new(BcEntity, BcEntities) of
				true ->
					EntitiesEventPid = bc_entities:event(BcEntities),
					gen_event:notify(EntitiesEventPid, {entity_spawned, BcEntity}),
					{ok, BcEntity};
				false ->
					{error_entity, "Error inserting entity."}
			end;
		false ->
			{error_collision, "Cannot insert collision."}
	end.

-spec entity_distance(BcEntity1 :: bc_entity:entity(), 
					  BcEntity2 :: bc_entity:entity()) -> integer().
entity_distance(BcEntity1, BcEntity2) ->
	%% TODO implement a better distance algorithm
	BcVertex1 = lists:nth(1, bc_entity:vertices(BcEntity1)),
	BcVertex2 = lists:nth(1, bc_entity:vertices(BcEntity2)),
	bc_vertex:distance(BcVertex1, BcVertex2).

-spec vertex_distance(BcEntity :: bc_entity:entity(), 
					  QueryBcVertex :: bc_vertex:vertex()) -> integer().
vertex_distance(BcEntity, QueryBcVertex) ->
	BcVertices = bc_entity:vertices(BcEntity),
	Distances = 
		lists:map(fun(EntityBcVertex) -> 
					bc_vertex:distance(EntityBcVertex, QueryBcVertex) 
				  end, BcVertices),
	lists:min(Distances).

-spec closest_entity_vertex(BcEntity :: bc_entity:entity(), 
							QueryBcVertex :: bc_vertex:vertex()) -> bc_vertex:vertex().
closest_entity_vertex(BcEntity, QueryBcVertex) ->
	BcVertices = bc_entity:vertices(BcEntity),
	DistVertices = 
		lists:map(fun(BcVertex) ->  
					Distance = 
						bc_entity_utils:vertex_distance(BcEntity, BcVertex),
					{Distance, BcVertex} 
				  end, BcVertices),
	FirstDistVertex = lists:nth(1, DistVertices),
	{_, ClosestBcVertex} =
		lists:foldl(fun({Dist, BcVertex} = DistVertex,
						{AccDist, AccBcVertex} = AccDistVertex) -> 
						case Dist < AccDist of
							true -> DistVertex;
							false -> AccDistVertex
						end
					end, FirstDistVertex, DistVertices),
	ClosestBcVertex.

-spec iolist_to_entity_type(EntityTypeStr :: io_list()) -> atom().
iolist_to_entity_type(EntityTypeStr) ->
	case EntityTypeStr of
		<<"champion">> -> champion;
		<<"demon">> -> demon;
		<<"chaosbeast">> -> chaosbeast;
		_ -> undefined
	end.

create_entity(BcCollision, BcPlayer, BcEntitySup, 
			  BcEntityConfig, Orientation, BcMap, BcEntities) ->
	Uuid = bc_collision:uuid(BcCollision),
	BcVertices = bc_collision:vertices(BcCollision),
	BaseUuidStr = uuid:uuid_to_string(Uuid),
	PlayerId = bc_player:id(BcPlayer),
	Team = bc_player:team(BcPlayer),
	EntityType = bc_entity_config:entity_type(BcEntityConfig),
	Health = bc_entity_config:health(BcEntityConfig),
	BcEntity = bc_entity:init(BaseUuidStr, PlayerId, Team, EntityType, 
				   			  Health, Health, Orientation, BcVertices),
	{ok, BcAiFsm} = supervisor:start_child(BcEntitySup, #{
		id => Uuid,
		start => {bc_ai_fsm, start_link, [BcEntity, BcEntities, BcMap]},
		restart => transient,
		type => worker,
		modules => [bc_ai_fsm]
	}),
	bc_entity:set_ai_fsm(BcAiFsm, BcEntity).
