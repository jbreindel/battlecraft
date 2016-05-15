
-module(battlecraft_game_fsm).
-behavior(gen_fsm).
-include("../include/bc_game_state.hrl").
-include("bc_game.hrl").

%% exported funcs
-export([start_link/1, player_join/3, player_out/2]).
-export([init/1, game_created/2, game_pending/2, game_started/2]).

%% state rec
-record(state, #{
					game_id,
					players
  				}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(GameId) ->
	gen_fsm:start_link(?MODULE, [GameId], []).

player_join(GameFsmPid, PlayerPid, Handle) ->
	gen_fsm:sync_send_event(GameFsmPid, {player_join, #{pid => PlayerPid, 
														handle => Handle}}).

player_out(GameFsmPid, PlayerId) ->
	gen_fsm:send_event(GameFsmPid, {player_out, #{player_id => PlayerId}}).

%%====================================================================
%% Gen_fsm callbacks
%%====================================================================

init(Privacy) ->
	Now = now(),
	GameId = bc_model:gen_id(game),
	Game = game#{id = GameId,
				 state = ?CREATED,
				 winner_id = 0,
				 is_private = Privacy,
				 created = Now,
				 modified = Now},
	case mnesia:sync_transaction(fun() -> mnesia:write(Game) end) of
		{atomic, Result} ->
			{ok, created, #state{game_id => GameId, 
								 players => dict:new()}};
		{aborted, Reason} ->
			{stop, Reason}
	end.

created({player_join, #{pid => Pid,
						handle => Handle}}, 
		State = state#{game_id = GameId, 
					   players = PlayerDict}) ->
	case save_player(Handle) of
		{ok, PlayerId} ->
			{ok, pending, State#state{players = dict:store(PlayerId, Pid, PlayerDict)}};
		{error, Reason} ->
			{ok, created, State}
	end.

pending({player_join, #{pid => Pid,
						handle => Handle}},
		State = state#{game_id = GameId,
					   players = PlayerDict}) ->
	case save_player(GameId, Handle) of
		{ok, PlayerId} ->
			case dict:size(PlayerDict) of
				Length when Length =:= 3 ->
					case update_game_state(GameId, ?STARTED) of
						{ok, _} ->
							%% TODO propagate game started event
							{next_state, started, State#state{players = 
																  dict:store(PlayerId, Pid, PlayerDict)}};
						{error, Reason} ->
							{next_state, pending, State}
					end;
				_ ->
					{next_state, pending, State}
			end;
		_ ->
			{next_state, pending, State}
	end.
%% pending({player_quit, {player_id, PlayerId}},
%% 			 State = state#{game_id = GameId, 
%% 							players = PlayerDict}) ->
%% 	UpdatedPlayerDict = dict:erase(PlayerDict, PlayerId),
%% 	case dict:size(UpdatedPlayerDict) of
%% 		Length when Length =:= 0 ->
%% 			case update_game_state(GameId, ?QUIT) of
%% 				{ok, _} ->
%% 					{}

%% game_started({player_out, OutPlayer}, State) ->
%% 	#state{game = Game} = State,
%% 	InPlayers = Game:players({out, equals, false}),
%% 	OutPlayer = Player:set(out, true),
%% 	OutPlayer:save(),
%% 	case length(InPlayers) of
%% 		Length when Length =:= 2 ->
%% 			[WinningPlayer|_] = lists:filter(
%% 				fun(Player) ->
%% 					Player:id() /= QuitPlayer:id()					
%% 				end, PlayerList),
%% 			WonGame = Game:set(winner_id, WinningPlayer:id()),
%% 			UpdatedGame = WonGame:set(state, ?WON),
%% 			case UpdatedGame:save() of
%% 				{ok, SavedGame} ->
%% 					{stop, normal, #state{game = SavedGame}};
%% 				{error, Error} ->
%% 					{stop, error, #state{game = UpdatedGame}}
%% 			end;
%% 		_ ->
%% 			{next_state, game_started, State}
%% 	end.

%%====================================================================
%% Internal functions
%%====================================================================

save_player(GameId, Handle) ->
	Now = now(),
	PlayerId = bc_model:gen_id(),
	Player = player#{player_id = PlayerId,
					 handle = Handle,
					 is_out = false,
					 created = Now,
					 modified = Now},
	GamePlayerAssoc = gp_assoc#{id = bc_model:gen_id(),
								gp = {GameId, PlayerId}},
	case mnesia:sync_transaction(fun() ->
										 mnesia:write(Player),
										 mnesia:write(GamePlayerAssoc)
								 end) of
		{atomic, Result} ->
			{ok, PlayerId};
		{aborted, Reason} ->
			{error, Reason}
	end.

remove_Player(PlayerId) ->
	case mnesia:sync_transaction(fun() -> 
										 mnesia:delete(player, PlayerId, write),
										 %%mnesia:delete(gp_assoc, _, _)
								 end) of
		{atomic, _} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}

update_game_state(GameId, GameState) ->
	case mnesia:sync_transaction(fun() -> 
										 [Game] = mnesia:wread(game, GameId), 
										 mnesia:write(Game#game{state = State}) 
								 end) of
		{atomic, Result} ->
			{ok, GameState};
		{aborted, Reason} ->
			{error, Reason}
	end.

