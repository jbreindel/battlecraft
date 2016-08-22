
-module(bc_player_model).
-include_lib("stdlib/include/qlc.hrl").
-include("bc_model.hrl").

-export([player_ids/1, 
		 in_player_ids/1,
		 game_players/1,
		 save/3,
		 update_out/2, 
		 delete/2]).
 
-spec player_ids(GameId :: integer()) -> [integer()].
player_ids(GameId) ->
	MatchSpec = ets:fun2ms(fun({_, _, GmId, _} = GpAssoc) 
								when GmId =:= GameId -> GpAssoc end),
	ResultList = mnesia:select(gp_assoc, MatchSpec),
	lists:map(fun(GpAssoc) -> GpAssoc#gp_assoc.player_id end, ResultList).

-spec in_player_ids(GameId :: integer()) -> [integer()].
in_player_ids(GameId) ->
	PlIds = player_ids(GameId),
	MatchSpec = [{{'_', PlId, false, '_', '_', '_'}, [], ['$_']} || PlId <- PlIds],
	ResultList = mnesia:select(player, MatchSpec),
	lists:map(fun(GpAssoc) -> GpAssoc#gp_assoc.player_id end, ResultList).

-spec game_players(GameIds :: [integer()]) -> {ok, PlayerDict :: dict:dict()} | {error, Reason :: string()}.
game_players(GameIds) ->
	case mnesia:sync_transaction(fun() -> 
		GpaQh = qlc:q([GpAssoc || GpAssoc <- mnesia:table(gp_assoc), 
								  lists:member(GpAssoc#gp_assoc.game_id, GameIds)]),
		PQh = qlc:q([Player || Player <- mnesia:table(player)]),
		GpQh = qlc:q([{GpAssoc#gp_assoc.game_id, Player} || Player <- PQh, GpAssoc <- GpaQh, 
															Player#player.id =:= GpAssoc#gp_assoc.player_id]),
		qlc:fold(fun({GameId, Player}, GpDict) ->
					case dict:find(GameId, GpDict) of
						{ok, Players} ->
							UpdatedPlayers = Players ++ [#{id => Player#player.id,
														  handle => Player#player.handle,
														  team => Player#player.team,
														  is_out => Player#player.is_out}],
							dict:store(GameId, UpdatedPlayers, GpDict);
						error ->
							dict:store(GameId, [#{id => Player#player.id,
												  handle => Player#player.handle,
												  team => Player#player.team,
												  is_out => Player#player.is_out}], GpDict)
					end
				 end, dict:from_list([{Gid, []} || Gid <- GameIds]), GpQh)	end) of
		{atomic, PlayerDict} ->
			{ok, PlayerDict};
		{error, Reason} = Error ->
			Error
	end.

-spec save(GameId :: integer(), 
		   Handle :: string(),
		   Team :: integer()) -> {ok, PlayerId :: integer()} | {error, Reason :: string()}.
save(GameId, Handle, Team) ->
	Now = erlang:system_time(seconds),
	PlayerId = bc_model:gen_id(player),
	Player = #player{id = PlayerId,
					 handle = Handle,
					 team = Team,
					 is_out = false,
					 created = Now,
					 modified = Now},
	GamePlayerAssoc = #gp_assoc{id = bc_model:gen_id(gp_assoc),
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

-spec update_out(PlayerId :: integer(), 
				 IsOut :: boolean()) -> ok | {error, Reason :: string()}.
update_out(PlayerId, IsOut) ->
	case mnesia:sync_transaction(fun() -> 
										 [Player] = mnesia:wread({player, PlayerId}),
										 mnesia:write(Player#player{is_out = IsOut, modified = erlang:system_time(seconds)})
								 end) of
		{atomic, Result} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

-spec delete(GameId :: integer(), 
			 PlayerId :: integer()) -> ok | {error, Reason :: string()}.
delete(GameId, PlayerId) ->
	MatchSpec = ets:fun2ms(fun({_, GpId, GmId, PlId}) 
							  when GmId =:= GameId andalso 
									   PlId =:= PlayerId -> GpId end),
	case mnesia:sync_transaction(fun() ->
										case mnesia:select(gp_assoc, MatchSpec, 1, read) of
											{[Id], 1} ->
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