
-module(bc_collision).

create(GameId) ->
	mnesia:create_table(GameId, [{ram_copies, [node()], 
		{storage_properties, [{ets, {write_concurrency, true}}]}]).
