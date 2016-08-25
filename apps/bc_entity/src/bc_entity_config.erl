
-module(bc_entity_config).

%% api exports
-export([init/1, 
		 entity_type/1, 
		 entity_class/1, 
		 health/1, 
		 damage/1, 
		 atk_speed/1,
		 range/1, 
		 move_speed/1]).

%%
% @doc type for storing entity configuration data
%%
-type entity_config() :: #{entity_type => atom(),
	  					   entity_class => atom(),
	  					   health => integer(),
	  					   damage => {integer(), integer()},
						   atk_speed => float(),
	  					   range => integer(),
	  					   move_speed => float()}.

%% type exports
-export_type([entity_config/0]).

-spec init(EntityTuple :: tuple()) -> entity_config().
init({EntityType, EntityClass, Health, DamageTuple, AtkSpeed, Range, MoveSpeed}) ->
	#{entity_type => EntityType,
	  entity_class => EntityClass,
	  health => Health,
	  damage => DamageTuple,
	  atk_speed => AtkSpeed,
	  range => Range,
	  move_speed => MoveSpeed}.

-spec entity_type(BcEntityConfig :: entity_config()) -> atom().
entity_type(BcEntityConfig) ->
	maps:get(entity_type, BcEntityConfig).

-spec entity_class(BcEntityConfig :: entity_config()) -> atom().
entity_class(BcEntityConfig) ->
	maps:get(entity_class, BcEntityConfig).

-spec health(BcEntityConfig :: entity_config()) -> integer().
health(BcEntityConfig) ->
	maps:get(health, BcEntityConfig).

-spec damage(BcEntityConfig :: entity_config()) -> {integer(), integer()}.
damage(BcEntityConfig) ->
	maps:get(damage, BcEntityConfig).

-spec atk_speed(BcEntityConfig :: entity_config()) -> float().
atk_speed(BcEntityConfig) ->
	maps:get(atk_speed, BcEntityConfig).

-spec range(BcEntityConfig :: entity_config()) -> integer().
range(BcEntityConfig) ->
	maps:get(range, BcEntityConfig).

-spec move_speed(BcEntityConfig :: entity_config()) -> float().
move_speed(BcEntityConfig) ->
	maps:get(move_speed, BcEntityConfig).
