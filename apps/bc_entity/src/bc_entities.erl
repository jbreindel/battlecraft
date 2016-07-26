
-module(bc_entities).
-include_lib("stdlib/include/qlc.hrl").

%% ets table options
-define(ETS_OPTIONS(Heir), [set, 
							public, 
							{heir, Heir, []}, 
							{write_concurrency, true}]).

-export([init/1, insert_new/2, insert/2, query/2, delete/2]).

%%====================================================================
%% API functions
%%====================================================================

-spec init(Heir :: pid()) -> ets:tab().
init(Heir) ->
	ets:new(entities, ?ETS_OPTIONS(Heir)).

-spec insert_new(Tab :: ets:tab(), 
				 BcEntity :: bc_entity:entity()) -> boolean().
insert_new(Tab, BcEntity) ->
	Row = bc_entity:to_tuple(BcEntity),
	ets:insert_new(Tab, Row).

-spec insert(Tab :: ets:tab(), 
			 BcEntity :: bc_entity:entity()) -> true.
insert(Tab, BcEntity) ->
	Row = bc_entity:to_tuple(BcEntity),
	ets:insert(Tab, Row).

-spec query(Tab :: ets:tab(), 
			Uuid :: uuid:uuid()) -> [bc_entity:entity()].
query(Tab, Uuid) when is_binary(Uuid) ->
	query(Tab, [Uuid]);
query(Tab, Uuids) when is_list(Uuids) ->
	qlc:eval(qlc:q([bc_entity:from_tuple(BcEntityTuple) || 
					  BcEntityTuple <- ets:table(Tab),
					  lists:member(element(1, BcEntityTuple), Uuids)])).

-spec delete(Tab :: ets:tab(), 
			 Uuid :: uuid:uuid()) -> true.
delete(Tab, Uuid) ->
	ets:delete(Tab, Uuid).