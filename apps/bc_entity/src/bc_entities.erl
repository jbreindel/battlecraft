
-module(bc_entities).

%% ets table options
-define(ETS_OPTIONS(Heir), [set, 
							public, 
							{heir, Heir, []}, 
							{write_concurrency, true}]).

-export([]).

%%====================================================================
%% API functions
%%====================================================================

init(Heir) ->
	ets:new(entities, ?ETS_OPTIONS(Heir)).

insert_new(Tab, BcEntity) ->
	Row = bc_entity:to_tuple(BcEntity),
	ets:insert_new(Tab, Row).