
-module(bc_entity).

%% API exports
-export([init/8, 
		 init/9, 
		 uuid_str/1,
		 uuid/1,
		 player_id/1,
		 set_player_id/2, 
		 team/1, 
		 set_team/2,
		 entity_type/1,
		 health/1, 
		 set_health/2,
		 max_health/1,
		 set_max_health/2,
		 ai_fsm/1,
		 set_ai_fsm/2,
		 orientation/1,
		 set_orientation/2,
		 vertices/1,
		 set_vertices/2,
		 to_collision/1,
		 to_tuple/1,
		 from_tuple/1,
		 serialize/1]).

%%
%% @doc base entity type for saving and transfering entities.
%%
-type entity() :: #{uuid_str => string(), 
					player_id => integer(), 
					team => integer(), 
					entity_type => integer(), 
					health => integer(), 
					max_health => integer(),
					orientation => atom(),
					vertices => [bc_vertex:vertex()],
					ai_fsm => pid() | undefined}.

%% type exports
-export_type([entity/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec init(UuidStr :: string(), 
		   PlayerId :: integer(), 
		   Team :: integer(), 
		   EntityType :: integer(), 
		   Health :: integer(),
		   MaxHealth :: integer(),
		   Orientation :: atom(),
		   Vertices :: [bc_vertex:vertex()]) -> entity().
init(UuidStr, PlayerId, Team, EntityType, Health, MaxHealth, Orientation, Vertices) ->
	init(UuidStr, PlayerId, Team, EntityType, Health, MaxHealth, Orientation, Vertices, undefined).

-spec init(UuidStr :: string(), 
		   PlayerId :: integer(), 
		   Team :: integer(), 
		   EntityType :: integer(), 
		   Health :: integer(),
		   MaxHealth :: integer(),
		   Orientation :: atom(),
		   Vertices :: [bc_vertex:vertex()],
		   AIFsm :: pid()) -> entity().
init(UuidStr, PlayerId, Team, EntityType, Health, MaxHealth, Orientation, Vertices, AIFsm) ->
	#{uuid_str => UuidStr,
	  player_id => PlayerId,
	  team => Team,
	  entity_type => EntityType,
	  health => Health,
	  max_health => MaxHealth,
	  orientation => Orientation,
	  vertices => Vertices,
	  ai_fsm => AIFsm}.

-spec uuid_str(BcEntity :: entity()) -> string().
uuid_str(BcEntity) ->
	maps:get(uuid_str, BcEntity).
	
-spec uuid(BcEntity :: entity()) -> uuid:uuid().
uuid(BcEntity) ->
	UuidStr = uuid_str(BcEntity),
	uuid:string_to_uuid(UuidStr).

-spec player_id(BcEntity :: entity()) -> integer().
player_id(BcEntity)->
	maps:get(player_id, BcEntity).

-spec set_player_id(PlayerId :: integer(), BcEntity :: entity()) -> entity().
set_player_id(PlayerId, BcEntity) ->
	maps:update(player_id, PlayerId, BcEntity).

-spec team(BcEntity :: entity()) -> integer().
team(BcEntity) ->
	maps:get(team, BcEntity).

-spec set_team(Team :: integer(), BcEntity :: entity()) -> entity().
set_team(Team, BcEntity) ->
	maps:update(team, Team, BcEntity).

-spec entity_type(BcEntity :: entity()) -> integer().
entity_type(BcEntity) ->
	maps:get(entity_type, BcEntity).

-spec health(BcEntity :: entity()) -> integer().
health(BcEntity) ->
	maps:get(health, BcEntity).

-spec set_health(BcEntity :: entity(), Health :: integer()) -> entity().
set_health(BcEntity, Health) ->
	maps:update(health, Health, BcEntity).

-spec max_health(BcEntity :: entity()) -> integer().
max_health(BcEntity) ->
	maps:get(max_health, BcEntity).

-spec set_max_health(BcEntity :: entity(), MaxHealth :: integer()) -> entity().
set_max_health(BcEntity, MaxHealth) ->
	maps:update(max_health, MaxHealth, BcEntity).
	
-spec orientation(BcEntity :: entity()) -> atom().
orientation(BcEntity) ->
	maps:get(orientation, BcEntity).
	
-spec set_orientation(BcEntity :: entity(), Orientation :: atom()) -> atom().
set_orientation(BcEntity, Orientation) ->
	maps:update(orientation, Orientation, BcEntity).
	
-spec vertices(BcEntity :: entity()) -> [bc_vertex:vertex()].
vertices(BcEntity) ->
	maps:get(vertices, BcEntity).

-spec set_vertices(BcVertices :: [bc_vertex:vertex()], BcEntity :: entity()) -> entity().
set_vertices(BcVertices, BcEntity) ->
	maps:update(vertices, BcVertices, BcEntity).

-spec ai_fsm(BcEntity :: entity()) -> pid() | undefined.
ai_fsm(BcEntity) ->
	maps:get(ai_fsm, BcEntity).

-spec set_ai_fsm(BcAiFsm :: pid(), BcEntity :: entity()) -> entity().
set_ai_fsm(BcAiFsm, BcEntity) ->
	maps:update(ai_fsm, BcAiFsm, BcEntity).

-spec to_collision(BcEntity :: entity()) -> bc_collision:collision().
to_collision(BcEntity) ->
	Uuid = uuid(BcEntity),
	Vertices = vertices(BcEntity),
	bc_collision:init(Uuid, Vertices).

-spec to_tuple(BcEntity :: entity()) -> tuple().
to_tuple(BcEntity) ->
	{uuid(BcEntity), 
	 player_id(BcEntity), 
	 team(BcEntity), 
	 entity_type(BcEntity), 
	 health(BcEntity), 
	 max_health(BcEntity),
	 orientation(BcEntity),
	 ai_fsm(BcEntity)}.

-spec from_tuple(tuple()) -> entity().
from_tuple(
	{Uuid, 
	 PlayerId, 
	 Team, 
	 EntityType, 
	 Health, 
	 MaxHealth,
	 Orientation,
	 AiFsm}) ->
	init(uuid:uuid_to_string(Uuid), PlayerId, Team, EntityType, 
		Health, MaxHealth, Orientation, undefined, AiFsm).

-spec serialize(BcEntity :: entity()) -> map().
serialize(BcEntity) ->
	UuidStr = uuid_str(BcEntity),
	BinUuidStr = binary:list_to_bin(UuidStr),
	UpdatedBcEntity = maps:update(uuid_str, BinUuidStr, BcEntity),
	maps:remove(ai_fsm, UpdatedBcEntity).
