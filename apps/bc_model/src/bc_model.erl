
-module(bc_model).
-include("bc_model.hrl").

%% API exports
-export([init/0,
		 init/1,
		 gen_id/1,
		 create_tables/2,
		 migrate/0,
		 rebuild/0]).

%% Ids - used to track sequence ids
-record('_ids_', {
			  %% type of record
			  type, 
			  %% id integer
			  id
	}).

%%====================================================================
%% API functions
%%====================================================================

init() ->
	init([node()]).

init(Nodes) ->
	mnesia:create_schema(Nodes),
	mnesia:start(),
	create_tables(Nodes, [#{name => '_ids_',
							attributes => record_info(fields, '_ids_')}]).

init_model() ->
	bc_model:init(),
	Tables = [#{name => player,
				attributes => record_info(fields, player)},
			  #{name => game,
				attributes => record_info(fields, game)},
			  #{name => gp_assoc,
				attributes => record_info(fields, gp_assoc)}],
	create_tables([node()], Tables).

gen_id(Tab) ->
	mnesia:dirty_update_counter('_ids_', Tab, 1).

create_tables(_, []) ->
	ok;
create_tables(Nodes, [Table|Tables]) ->
	mnesia:create_table(maps:get(name, Table),
		[{ disc_copies, Nodes },
		 { attributes, maps:get(attributes, Table) }]),
	create_tables(Nodes, Tables).

migrate() ->
    mnesia:stop(),
    init().

rebuild() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    init().
