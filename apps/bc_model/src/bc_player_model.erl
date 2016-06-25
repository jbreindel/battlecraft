
-module(bc_player_model).
-include("bc_model.hrl").

-export([save/2, 
		 player_ids/1, 
		 in_player_ids/1, 
		 update_out/2, 
		 delete/2]).

-spec save(GameId :: integer(), 
		   Handle :: string()) -> {ok, PlayerId :: integer()} | {error, Reason :: string()}.
save(GameId, Handle) ->
	Now = erlang:system_time(seconds),
	PlayerId = bc_model:gen_id(player),
	Player = #player{id = PlayerId,
					 handle = Handle,
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

-spec update_out(PlayerId :: integer(), 
				 IsOut :: boolean()) -> ok | {error, Reason :: string()}.
update_out(PlayerId, IsOut) ->
	case mnesia:sync_transaction(fun() -> 
										 [Player] = mnesia:wread(player, PlayerId),
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