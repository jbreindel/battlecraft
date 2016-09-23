
-module(bc_entity_config).

%% api exports
-export([init/1, 
		 entity_type/1, 
		 entity_class/1, 
		 size/1,
		 health/1, 
		 damage/1, 
		 attack_speed/1,
		 range/1, 
		 move_speed/1,
		 cost/1]).

%%
% @doc type for storing entity configuration data
%%
-type entity_config() :: #{entity_type => atom(),
	  					   entity_class => atom(),
						   size => integer(),
	  					   health => integer(),
	  					   damage => {integer(), integer()},
						   attack_speed => float(),
	  					   range => integer(),
	  					   move_speed => float(),
						   cost => integer()}.

%% type exports
-export_type([entity_config/0]).

-spec init(EntityTuple :: tuple()) -> entity_config().
init({EntityType, EntityClass, Size, Health, DamageTuple, AtkSpeed, Range, MoveSpeed, Cost}) ->
	#{entity_type => EntityType,
	  entity_class => EntityClass,
	  size => Size,
	  health => Health,
	  damage => DamageTuple,
	  attack_speed => AtkSpeed,
	  range => Range,
	  move_speed => MoveSpeed,
	  cost => Cost}.

-spec entity_type(BcEntityConfig :: entity_config()) -> atom().
entity_type(BcEntityConfig) ->
	maps:get(entity_type, BcEntityConfig).

-spec entity_class(BcEntityConfig :: entity_config()) -> atom().
entity_class(BcEntityConfig) ->
	maps:get(entity_class, BcEntityConfig).

-spec size(BcEntityConfig :: entity_config()) -> integer().
size(BcEntityConfig) ->
	maps:get(size, BcEntityConfig).

-spec health(BcEntityConfig :: entity_config()) -> integer().
health(BcEntityConfig) ->
	maps:get(health, BcEntityConfig).

-spec damage(BcEntityConfig :: entity_config()) -> {integer(), integer()}.
damage(BcEntityConfig) ->
	maps:get(damage, BcEntityConfig).

-spec attack_speed(BcEntityConfig :: entity_config()) -> float().
attack_speed(BcEntityConfig) ->
	maps:get(attack_speed, BcEntityConfig).

-spec range(BcEntityConfig :: entity_config()) -> integer().
range(BcEntityConfig) ->
	maps:get(range, BcEntityConfig).

-spec move_speed(BcEntityConfig :: entity_config()) -> float().
move_speed(BcEntityConfig) ->
	maps:get(move_speed, BcEntityConfig).

-spec cost(BcEntityConfig :: entity_config()) -> integer().
cost(BcEntityConfig) ->
	maps:get(cost, BcEntityConfig).
