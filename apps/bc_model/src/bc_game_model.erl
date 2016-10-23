
-module(bc_game_model).
-include_lib("stdlib/include/qlc.hrl").
-include("bc_model.hrl").

-export([get_games/3,
		 save/3, 
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
			GameIds = lists:map(fun(#game{id = Id}) -> Id end, Records),
			case bc_player_model:game_players(GameIds) of
				{ok, PlayerDict} ->
					{ok, qlc:eval(qlc:q([#{id => GameRec#game.id,
										   state => GameRec#game.state,
										   players => Players,
										   winner_id => GameRec#game.winner_id,
										   is_private => GameRec#game.is_private,
										   max_players => GameRec#game.max_players,
										   created => GameRec#game.created,
										   modified => GameRec#game.modified} || GameRec <- Records, 
																				 {GameId, Players} <- dict:to_list(PlayerDict),
																				 GameId =:= GameRec#game.id]))};
				{error, Reason} = PlayerError ->
					PlayerError
			end;
		{error, Reason} = GameError ->
			GameError
	end.
	
-spec save(Privacy :: integer(), 
		   MaxPlayers :: integer(), 
		   State :: integer()) -> {ok, GameId :: integer()} | 
									  {error, Reason :: string()}.
save(Privacy, MaxPlayers, State) ->
	Now = erlang:system_time(seconds),
	GameId = bc_model:gen_id(game),
	Game = #game{id = GameId,
				 state = State,
				 winner_id = 0,
				 is_private = Privacy,
				 max_players = MaxPlayers,
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
										 [Game] = mnesia:wread({game, GameId}), 
										 mnesia:write(Game#game{state = GameState, 
																modified = erlang:system_time(seconds)}) 
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
									[Game] = mnesia:wread({game, GameId}),
									mnesia:write(Game#game{state = State, 
														   winner_id = WinnerId, 
														   modified = erlang:system_time(seconds)})
								 end) of
		{atomic, Result} ->
			{ok, won};
		{aborted, Reason} ->
			{error, Reason}
	end.
	

