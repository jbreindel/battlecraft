
-module(bc_entity).

%% API exports
-export([create/5, 
		 create/6, 
		 uuid/1, 
		 player_id/1, 
		 team/1, 
		 entity_type/1, 
		 health/1, 
		 ai_fsm/1, 
		 damage/1,
		 to_tuple/1]).

%%
%% @doc base entity type for saving and transfering entities.
%%
-type entity() :: #{uuid => uuid(), 
					player_id => integer(), 
					team => integer(), 
					entity_type => integer(), 
					health => integer(), 
					ai_fsm => pid() | undefined}.

%% type exports
-export_type([entity/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec create(Uuid :: uuid(), 
			 PlayerId :: integer(), 
			 Team :: integer(), 
			 EntityType :: integer(), 
			 Health :: integer()) -> entity().
create(Uuid, PlayerId, Team, EntityType, Health) ->
	create(Uuid, PlayerId, Team, EntityType, Health, undefined);

-spec create(Uuid :: uuid(), 
			 PlayerId :: integer(), 
			 Team :: integer(), 
			 EntityType :: integer(), 
			 Health :: integer(),
			 AIFsm :: pid()) -> entity().
create(Uuid, PlayerId, Team, EntityType, Health, AIFsm) ->
	#{uuid => Uuid,
	  player_id => PlayerId,
	  team => Team,
	  entity_type => EntityType,
	  health => Health,
	  ai_fsm => AIFsm}.

-spec uuid(BcEntity :: entity()) -> uuid().
uuid(BcEntity) ->
	maps:get(uuid, BcEntity).

-spec player_id(BcEntity :: entity()) -> integer().
player_id(BcEntity)->
	maps:get(player_id, BcEntity).

-spec team(BcEntity :: entity()) -> integer().
team(BcEntity) ->
	maps:get(team, BcEntity).

-spec entity_type(BcEntity :: entity()) -> integer().
entity_type(BcEntity) ->
	maps:get(entity_type, BcEntity).

-spec health(BcEntity :: entity()) -> integer().
health(BcEntity) ->
	maps:get(health, BcEntity).

-spec ai_fsm(BcEntity :: entity()) -> pid() | undefined.
ai_fsm(BcEntity) ->
	maps:get(ai_fsm, BcEntity).

-spec damage(BcEntity :: entity(), Damage :: integer()) -> entity().
damage(BcEntity, Damage) ->
	Health = maps:get(BcEntity, health),
	maps:update(health, Health - Damage, BcEntity).

-spec to_tuple(BcEntity :: entity()) -> tuple().
to_tuple(BcEntity) ->
	{uuid(BcEntity), 
	 player_id(BcEntity), 
	 team(BcEntity), 
	 entity_type(BcEntity), 
	 health(BcEntity), 
	 ai_fsm(BcEntity)}.
