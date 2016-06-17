
-module(bc_game_model).
-include("bc_model.hrl").

-export(win_game/2, update_state/2).

save(Privacy) ->
	Now = now(),
	GameId = bc_model:gen_id(game),
	Game = #game{id = GameId,
				 state = ?PENDING,
				 winner_id = 0,
				 is_private = Privacy,
				 created = Now,
				 modified = Now},
	case mnesia:sync_transaction(fun() -> mnesia:write(Game) end) of
		{atomic, _} ->
			{ok, GameId};
		{aborted, Reason} ->
			{error, Reason}
	end.

win(GameId, WinnerId) ->
	case mnesia:sync_transaction(fun() -> 
									[Game] = mnesia:wread(game, GameId),
									mnesia:write(Game#game{state = ?WON, 
														   winner_id = WinnerId, 
														   modified = now()})
								 end) of
		{atomic, Result} ->
			{ok, won};
		{aborted, Reason} ->
			{error, Reason}
	end.

update_state(GameId, GameState) ->
	case mnesia:sync_transaction(fun() -> 
										 [Game] = mnesia:wread(game, GameId), 
										 mnesia:write(Game#game{state = GameState, 
																modified = now()}) 
								 end) of
		{atomic, Result} ->
			{ok, GameState};
		{aborted, Reason} ->
			{error, Reason}
	end.
