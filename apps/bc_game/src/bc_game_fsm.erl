
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

player_quit(GameFsmPid, PlayerId) ->
	gen_fsm:send_event(GameFsmPid, {player_quit, #{pid => PlayerPid,
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
		S = State#state{game_id = GameId, 
					   players = PlayerDict}) ->
	case save_player(Handle) of
		{ok, PlayerId} ->
			{ok, pending, State#state{players = dict:store(PlayerId, Pid, PlayerDict)}};
		{error, Reason} ->
			{ok, created, State}
	end.

pending({player_join, #{pid => Pid,
						handle => Handle}},
		S = State#state{game_id = GameId,
					   players = PlayerDict}) ->
	case save_player(GameId, Handle) of
		{ok, PlayerId} ->
			UpdatedPlayerDict = dict:store(PlayerId, Pid, PlayerDict),
			on_players_changed(GameId, UpdatedPlayerDict);
		_ ->
			{next_state, pending, State}
	end;
pending({player_quit, {player_id, PlayerId}},
			 S = State#state{game_id = GameId, 
							players = PlayerDict}) ->
	case remove_player(GameId, PlayerId) of
		ok ->
			UpdatedPlayerDict = dict:erase(PlayerDict, PlayerId),
			on_players_changed(GameId, UpdatedPlayerDict);
		{error, Reason} ->
			{next_state, pending, State}
	end.

started({player_out, OutPlayerId}, 
		S = State#state{game_id = GameId, players = PlayerDict}) ->
	case update_out_player(GameId, OutPlayerId) of
		ok ->
			on_player_out(GameId, PlayerDict);
		{error, Reason} ->
			%% TODO dispatch game error
			{stop, {error, Reason}, S}
	end.
started({player_quit, QuitPlayerId},
		S = State#state{game_id = GameId, players = PlayerDict}) ->
	case update_out_player(GameId, QuitPlayerId) of
		ok ->
			UpdatedPlayerDict = dict:erase(QuitPlayerId, PlayerDict),
			on_player_out(GameId, UpdatedPlayerDict);
		{error, Reason} ->
			%% TODO dispatch game error
			{stop, {error, Reason}, S}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

on_players_changed(GameId, UpdatedPlayerDict) ->
	case dict:size(PlayerDict) of
		Length when Length =:= 4 ->
			start_game(GameId, UpdatedPlayerDict);
		Length when Length =:= 0 ->
			quit_game(GameId, UpdatedPlayerDict);
		_ ->
			{next_state, pending, #state{game_id = GameId,
										 players = UpdatedPlayerDict}}
	end.

on_player_out(GameId, PlayerDict) ->
	State = #state{game_id = GameId, players = PlayerDict},
	InPlayers = in_players(PlayerDict),
	case length(InPlayers) of
		Length when Length =:= 1 ->
			[InPlayer] = InPlayers,
			case win_game(GameId, InPlayer#player.id) of
				ok ->
					%% TODO dispatch game won
					{stop, ok, State};
				{error, Reason} ->
					%% TODO dispatch game error
					{stop, {error, Reason}, State}
			end;
		_ ->
			{next_state, started, State}
	end.

start_game(GameId, UpdatedPlayerDict) ->
	UpdatedState = #state{game_id = GameId, players = UpdatedPlayerDict},
	case update_game_state(GameId, ?STARTED) of
		{ok, _} ->
			%% TODO propagate game started event
			{next_state, started, UpdatedState};
		{error, Reason} ->
			%% TODO dispatch game error
			{stop, {error, Reason}, UpdatedState}
	end.

quit_game(GameId, UpdatedPlayerDict) ->
	UpdatedState = #state{game_id = GameId, players = UpdatedPlayerDict},
	case update_game_state(GameId, ?QUIT) of
		{ok, _} ->
			%% TODO remove from active games
			{stop, ok, UpdatedState};
		{error, Reason} ->
			%% TODO dispatch game error
			{stop, {error, Reason}, UpdatedState}
	end.

win_game(GameId, WinnerId) ->
	case mnesia:sync_transaction(fun() -> 
									[Game] = mnesia:wread(game, GameId),
									mnesia:write(Game#game{state = ?WON, winner_id = WinnerId})
								 end) of
		{atomic, Result} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

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

update_out_player(GameId, PlayerId) ->
	case mnesia:sync_transaction(fun() -> 
										 [Player] = mnesia:wread(player, PlayerId),
										 mnesia:write(Player#player{is_out = true, modified = now()})
								 end) of
		{atomic, Result} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

in_players(PlayerDict) ->
	PlIds = dict:fetch_keys(PlayerDict),
	MatchSpec = [{{'_',PlId,false,'_','_','_'},[],['$_']} || PlId <- PlIds],
	mnesia:select(player, MatchSpec).
		
remove_player(GameId, PlayerId) ->
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

update_game_state(GameId, GameState) ->
	case mnesia:sync_transaction(fun() -> 
										 [Game] = mnesia:wread(game, GameId), 
										 mnesia:write(Game#game{state = State, modified = now()}) 
								 end) of
		{atomic, Result} ->
			{ok, GameState};
		{aborted, Reason} ->
			{error, Reason}
	end.
