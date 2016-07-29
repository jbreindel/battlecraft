
-module(bc_entity_config).

%% api exports
-export([init/1, 
		 entity_type/1, 
		 entity_class/1, 
		 health/1, 
		 damage/1, 
		 range/1, 
		 speed/1]).

%%
% @doc type for storing entity configuration data
%%
-type entity_config() :: #{entity_type => atom(),
	  					   entity_class => atom(),
	  					   health => integer(),
	  					   damage => {integer(), integer()},
	  					   range => integer(),
	  					   speed => float()}.

%% type exports
-export_type([entity_config/0]).

-spec init(EntityTuple :: tuple()) -> entity_config().
init({EntityType, EntityClass, Health, DamageTuple, Range, Speed}) ->
	#{entity_type => EntityType,
	  entity_class => EntityClass,
	  health => Health,
	  damage => DamageTuple,
	  range => Range,
	  speed => Speed}.

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

-spec range(BcEntityConfig :: entity_config()) -> integer().
range(BcEntityConfig) ->
	maps:get(range, BcEntityConfig).

-spec speed(BcEntityConfig :: entity_config()) -> float().
speed(BcEntityConfig) ->
	maps:get(speed, BcEntityConfig).
