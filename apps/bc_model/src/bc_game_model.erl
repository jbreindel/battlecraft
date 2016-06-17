
-module(bc_game_model).
-include("bc_model.hrl").

-export(save/1, 
		win_game/2, 
		update_state/2).

-spec save(Privacy :: integer()) -> {ok, GameId :: integer()} | 
									{error, Reason :: string()}.
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

-spec win(GameId :: integer(), 
		  WinnerId :: integer()) -> {ok, won} | {error, Reason :: string()}.
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

-spec update_state(GameId :: integer(), 
				   GameState :: integer()) -> {ok, GameState :: integer()} | 
				   							  {error, Reason :: string()}.
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
