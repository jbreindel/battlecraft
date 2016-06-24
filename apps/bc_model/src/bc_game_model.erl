
-module(bc_game_model).
-include_lib("stdlib/include/qlc.hrl").
-include("bc_model.hrl").

-export([get_games/3,
		 save/2, 
		 win/3, 
		 update_state/2]).
		
-spec get_games(QueryState :: integer(), 
				Offset :: integer, 
				Limit :: integer()) -> {ok, Games :: [map()]} | {error, Reason :: string()}.
get_games(QueryState, Offset, Limit) ->
	case bc_query_util:mnesia_query(fun() -> 
			qlc:cursor(
				qlc:q([GameRecord || #game{state = State} = GameRecord <- mnesia:table(game), 
					State =:= QueryState])
			)
		end, Offset, Limit) of
		{atomic, Records} ->
			{ok, lists:map(fun(GameRec) -> 
							#{id => GameRec#game.id,
							  state => GameRec#game.state,
							  is_private => GameRec#game.is_private,
							  created => GameRec#game.created,
							  modified => GameRec#game.modified} 
					  end, Records)};
		{error, Reason} = Error ->
			Error
	end.
	
-spec save(Privacy :: integer(), State :: integer()) -> 
		  {ok, GameId :: integer()} | {error, Reason :: string()}.
save(Privacy, State) ->
	Now = now(),
	GameId = bc_model:gen_id(game),
	Game = #game{id = GameId,
				 state = State,
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

-spec win(GameId :: integer(), 
		  WinnerId :: integer(), 
		  State :: integer()) -> {ok, won} | {error, Reason :: string()}.
win(GameId, WinnerId, State) ->
	case mnesia:sync_transaction(fun() -> 
									[Game] = mnesia:wread(game, GameId),
									mnesia:write(Game#game{state = State, 
														   winner_id = WinnerId, 
														   modified = now()})
								 end) of
		{atomic, Result} ->
			{ok, won};
		{aborted, Reason} ->
			{error, Reason}
	end.
	

