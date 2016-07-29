
-module(bc_entity_config).

export([init/1]).

init({EntityType, EntityClass, Health, DamageTuple, Range, Speed}) ->
	#{entity_type => EntityType,
	  entity_class => EntityClass,
	  health => Health,
	  damage => DamageTuple,
	  range => Range,
	  speed => Speed}.

entity_type(BcEntityConfig) ->
	maps:get(entity_type, BcEntityConfig).

entity_class(BcEntityConfig) ->
	maps:get(entity_class, BcEntityConfig).

health(BcEntityConfig) ->
	maps:get(health, BcEntityConfig).

damage(BcEntityConfig) ->
	maps:get(damage, BcEntityConfig).

range(BcEntityConfig) ->
	maps:get(range, BcEntityConfig).

speed(BcEntityConfig) ->
	maps:get(speed, BcEntityConfig).
