
-module(bc_entities).
-include_lib("stdlib/include/qlc.hrl").

%% ets table options
-define(ETS_OPTIONS(Heir), [set, 
							public, 
							{heir, Heir, []}, 
							{write_concurrency, true}]).

-export([init/1, 
		 entity_config/2,
		 table/1,
		 event/1,
		 insert_new/2, 
		 insert/2, 
		 query/2, 
		 query_type/2,
		 exists/2,
		 delete/2]).

%%
% @doc entities structure includes gen_event along with ets table.
%%
-type entities() :: #{entities_config => dict:dict(atom(), bc_entity_config:entity_config()),
					  entities_event => pid(),
					  entities_tab => ets:tid()}.

%% decl types
-export_type([entities/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec init(Heir :: pid()) -> entities().
init(Heir) ->
	{ok, EntitiesEventPid} = supervisor:start_child(Heir, #{
		id => bc_entities_event,
		start => {gen_event, start_link, []},
		modules => [gen_event]
	}),
	#{entities_config => load_entities_config(),
	  entities_event => EntitiesEventPid,
	  entities_tab => ets:new(entities, ?ETS_OPTIONS(Heir))}.

-spec entity_config(EntityType :: atom(), 
					BcEntities :: entities()) -> dict:dict(atom(), bc_entity_config:entity_config()).
entity_config(EntityType, #{entities_config := EntitiesConfig}) ->
	dict:find(EntityType, EntitiesConfig).

-spec table(BcEntities :: entities()) -> ets:tid().
table(#{entities_tab := Tab}) ->
	Tab.

-spec event(BcEntities :: entities()) -> pid().
event(#{entities_event := EntitiesEventPid}) ->
	EntitiesEventPid.

-spec insert_new(BcEntity :: bc_entity:entity(),
				 BcEntities :: entities()) -> boolean().
insert_new(BcEntity, #{entities_tab := Tab}) ->
	Row = bc_entity:to_tuple(BcEntity),
	ets:insert_new(Tab, Row).

-spec insert(BcEntity :: bc_entity:entity(),
			 BcEntities :: entities()) -> true.
insert(BcEntity, #{entities_tab := Tab}) ->
	Row = bc_entity:to_tuple(BcEntity),
	ets:insert(Tab, Row).

-spec query(Uuid :: uuid:uuid(),
			BcEntities :: entities()) -> [bc_entity:entity()].
query(Uuid, BcEntities) when is_binary(Uuid) ->
	query([Uuid], BcEntities);
query(Uuids, #{entities_tab := Tab}) when is_list(Uuids) ->
	qlc:eval(qlc:q([bc_entity:from_tuple(BcEntityTuple) || 
					  BcEntityTuple <- ets:table(Tab),
					  lists:member(element(1, BcEntityTuple), Uuids)])).

-spec query_type(EntityType :: atom(), 
				 BcEntities :: entities()) -> [bc_entity:entity()].
query_type(EntityType, #{entities_tab := Tab}) ->
	qlc:eval(qlc:q([bc_entity:from_tuple(BcEntityTuple) ||
					  BcEntityTuple <- ets:table(Tab),
					  element(5, BcEntityTuple) =:= EntityType])).

-spec exists(Uuid :: uuid:uuid(), BcEntities :: entities()) -> boolean().
exists(Uuid, BcEntities) ->
	case query(Uuid, BcEntities) of
		QueryRes when length(QueryRes) > 0 ->
			true;
		[] ->
			false
	end.

-spec delete(Uuid :: uuid:uuid(),
			 BcEntities :: entities()) -> true.
delete(Uuid, #{entities_tab := Tab}) ->
	ets:delete(Tab, Uuid).

%%====================================================================
%% Internal functions
%%====================================================================

load_entities_config() ->
	EntitiesConfigFile = filename:join([code:priv_dir(bc_entity), "entities.config"]),
	{ok, EntityConfigs} = file:consult(EntitiesConfigFile),
	EntityTypeConfigs = lists:map(fun(EntityConfig) -> 
									{element(1, EntityConfig), bc_entity_config:init(EntityConfig)} 
								  end, EntityConfigs),
	dict:from_list(EntityTypeConfigs).
