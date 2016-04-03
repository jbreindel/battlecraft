
-module(bc_model).
-include("../include/bc_model.hrl").

%% API exports
-export([init/0, 
		 init/1, 
		 migrate/0, 
		 rebuild/0]).

%%====================================================================
%% API functions
%%====================================================================

init() ->
	init([node()]).

init(Nodes) ->
	mnesia:create_schema(Nodes),
	mnesia:start(),
	create_tables(Nodes).

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

create_tables(Nodes) ->
	mnesia:create_table('_ids_',
		[{ disc_copies, Nodes },
		 { attributes, record_info(fields, '_ids_') }]),
	mnesia:create_table(player,
		[{ disc_copies, Nodes },
		 { attributes, record_info(fields, player) }]),
	mnesia:create_table(game,
		[{ disc_copies, Nodes },
		 { attributes, record_info(fields, game) }]),
	mnesia:create_table(gp_assoc,
		[{ disc_copies, Nodes },
		 { attributes, record_info(fields, gp_assoc) }]).
