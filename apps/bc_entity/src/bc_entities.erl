
-module(bc_entities).
-include_lib("stdlib/include/qlc.hrl").

%% ets table options
-define(ETS_OPTIONS(Heir), [set, 
							public, 
							{heir, Heir, []}, 
							{write_concurrency, true}]).

-export([init/1, 
		 table/1,
		 event/1,
		 insert_new/2, 
		 insert/2, 
		 query/2, 
		 delete/2]).

%%
% @doc entities structure includes gen_event along with ets table.
%%
-type bc_entities() :: #{entities_event => pid(),
						 entities_tab => ets:tid()}.

%%====================================================================
%% API functions
%%====================================================================

-spec init(Heir :: pid()) -> bc_entities().
init(Heir) ->
	#{entities_event => supervisor:start_child(Heir, #{
		id => bc_entities_event,
		start => {gen_event, start_link, []},
		modules => [gen_event]
	  }),
	  entities_pid => ets:new(entities, ?ETS_OPTIONS(Heir))}.

-spec table(BcEntities :: bc_entities()) -> ets:tid().
table(#{entities_tab := Tab}) ->
	Tab.

-spec event(BcEntities :: bc_entities()) -> pid().
event(#{entities_event := EntitiesEventPid}) ->
	EntitiesEventPid.

-spec insert_new(BcEntities :: bc_entities(), 
				 BcEntity :: bc_entity:entity()) -> boolean().
insert_new(#{entities_tab := Tab}, BcEntity) ->
	Row = bc_entity:to_tuple(BcEntity),
	ets:insert_new(Tab, Row).

-spec insert(BcEntities :: bc_entities(), 
			 BcEntity :: bc_entity:entity()) -> true.
insert(#{entities_tab := Tab}, BcEntity) ->
	Row = bc_entity:to_tuple(BcEntity),
	ets:insert(Tab, Row).

-spec query(BcEntities :: bc_entities(), 
			Uuid :: uuid:uuid()) -> [bc_entity:entity()].
query(BcEntities, Uuid) when is_binary(Uuid) ->
	query(BcEntities, [Uuid]);
query(#{entities_tab := Tab}, Uuids) when is_list(Uuids) ->
	qlc:eval(qlc:q([bc_entity:from_tuple(BcEntityTuple) || 
					  BcEntityTuple <- ets:table(Tab),
					  lists:member(element(1, BcEntityTuple), Uuids)])).

-spec delete(BcEntities :: bc_entities(), 
			 Uuid :: uuid:uuid()) -> true.
delete(#{entities_tab := Tab}, Uuid) ->
	ets:delete(Tab, Uuid).