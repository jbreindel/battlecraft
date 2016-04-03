
-module(bc_model).
-include("../include/bc_model.hrl").

%% API exports
-export([init/0, init/1]).

%%====================================================================
%% API functions
%%====================================================================

init() ->
	init([node()]).

init(Nodes) ->
	mnesia:create_schema(Nodes),
	mnesia:start(),
	ExistingTables = mnesia:system_info(tables),
	Tables = ['_ids_', player, game, gp_assoc] -- ExistingTables,
	create_tables(Tables).

migrate() ->
    mnesia:stop(),
    init().

rebuild() ->
    mnesia:stop(),
    mnesia:delete_schema([node()]),
    init().

%%====================================================================
%% Internal functions
%%====================================================================

create_tables(Nodes, []) ->
	ok;
create_tables(Nodes, [Table|Tables]) ->
	mnesia:create_table(Table, 
		[{ disc_copies, Nodes },
		 { attributes, record_info(Table) }]).
