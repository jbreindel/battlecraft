
-module(bc_player_model).
-include("bc_model.hrl").

save(GameId, Handle) ->
	Now = now(),
	PlayerId = bc_model:gen_id(),
	Player = #player{id = PlayerId,
					 handle = Handle,
					 is_out = false,
					 created = Now,
					 modified = Now},
	GamePlayerAssoc = #gp_assoc{id = bc_model:gen_id(),
								game_id = GameId,
								player_id = PlayerId},
	case mnesia:sync_transaction(fun() ->
										 mnesia:write(Player),
										 mnesia:write(GamePlayerAssoc)
								 end) of
		{atomic, Result} ->
			{ok, PlayerId};
		{aborted, Reason} ->
			{error, Reason}
	end.

player_ids(GameId) ->
	MatchSpec = ets:fun2ms(fun({_, _, GmId, _} = GpAssoc) 
								when GmId =:= GameId -> GpAssoc end),
	ResultList = mnesia:select(gp_assoc, MatchSpec),
	lists:map(fun(GpAssoc) -> GpAssoc#gp_assoc.player_id end, ResultList).

in_player_ids(GameId) ->
	PlIds = player_ids(GameId),
	MatchSpec = [{{'_', PlId, false, '_', '_', '_'}, [], ['$_']} || PlId <- PlIds],
	ResultList = mnesia:select(player, MatchSpec),
	lists:map(fun(GpAssoc) -> GpAssoc#gp_assoc.player_id end, ResultList).

update_out(PlayerId, IsOut) ->
	case mnesia:sync_transaction(fun() -> 
										 [Player] = mnesia:wread(player, PlayerId),
										 mnesia:write(Player#player{is_out = IsOut, modified = now()})
								 end) of
		{atomic, Result} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

delete(GameId, PlayerId) ->
	MatchSpec = ets:fun2ms(fun({_, _, GmId, PlId} = GP) 
							  when GmId =:= GameId andalso 
									   PlId =:= PlayerId -> GP end),
	case mnesia:sync_transaction(fun() ->
										case mnesia:select(gp_assoc, MatchSpec, 1, read) of
											{[GPAssoc], 1} ->
												Id = GPAssoc#gp_assoc.id,
												mnesia:delete(gp_assoc, Id, write);
											'$end_of_table' ->
												ok
										end,
										mnesia:delete(player, PlayerId, write)
								 end) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.