
-module(bc_game_fsm).
-behavior(gen_fsm).
-include("../include/bc_game_state.hrl").
-include("bc_game.hrl").

%% exported funcs
-export([start_link/1, player_join/3, player_quit/2, player_out/2]).
-export([init/1, pending/2, started/2]).

%% state rec
-record(state, {
				game_id,
				players
  				}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(GameId) ->
	gen_fsm:start_link(?MODULE, [GameId], []).

player_join(GameFsmPid, PlayerPid, Handle) ->
	gen_fsm:sync_send_event(GameFsmPid, 
							{player_join, #{player_pid => PlayerPid,
											handle => Handle}}).

player_quit(GameFsmPid, PlayerId) ->
	gen_fsm:send_event(GameFsmPid, {player_quit, PlayerId}).

player_out(GameFsmPid, PlayerId) ->
	gen_fsm:send_event(GameFsmPid, {player_out, PlayerId}).

%%====================================================================
%% Gen_fsm callbacks
%%====================================================================

init(GameId) ->
	{ok, created, #state{game_id = GameId,
						 players = dict:new()}}.

pending({player_join, #{player_pid := PlayerPid, 
						handle := Handle}},
		State = #state{game_id = GameId,
					   players = Players}) ->
	case save_player(GameId, Handle) of
		{ok, PlayerId} = Reply ->
			%% TODO dispatch player joined event
			UpdatedPlayers = dict:store(PlayerId, PlayerPid, Players),
			%% TODO add monitor for player pid
			UpdatedState = State#state{players = UpdatedPlayers},
			case pending_players_changed(GameId, UpdatedPlayers) of
				{ok, started} ->
					{reply, Reply, started, UpdatedState};
				{ok, pending} ->
					{reply, Reply, pending, UpdatedState};
				{error, Reason} = Error ->
					{stop, Error, Error, UpdatedState};
				_ ->
					{stop, illegal_state, {error, illegal_state}, UpdatedState}
			end;
		{error, Reason} = Error ->
			{reply, Error, pending, State}
	end;
pending({player_quit, PlayerId},
		State = #state{game_id = GameId,
					   players = Players}) ->
	case remove_player(GameId, PlayerId) of
		ok ->
			UpdatedPlayers = dict:erase(PlayerId, Players),
			UpdatedState = State#state{players = UpdatedPlayers},
			case pending_players_changed(GameId, UpdatedPlayers) of
				{ok, quit} ->
					{stop, quit, UpdatedState};
				{ok, pending} ->
					{next_state, pending, UpdatedState};
				{error, Reason} = Error ->
					{stop, Error, UpdatedState};
				_ ->
					{stop, illegal_state, UpdatedState}
			end;
		{error, Reason} = Error ->
			{stop, Error, State}
	end.

started({_, OutPlayerId}, 
		State = #state{game_id = GameId,
					   players = Players}) ->
	case update_out_player(OutPlayerId) of
		ok ->
			InPlayers = in_players(GameId),
			case length(InPlayers) of
				Length when Length =:= 1 ->
					[InPlayer] = in_players(GameId),
					case win_game(GameId, InPlayer#player.id) of
						{ok, won} ->
							{stop, won, State};
						{error, Reason} = Error ->
							{stop, Error, State}
					end;
				_ ->
					{next_state, started, State}
			end;
		{error, Reason} = Error ->
			{stop, Error, State}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

pending_players_changed(GameId, Players) ->
	case dict:size(Players) of
		Length when Length =:= 4 ->
			start_game(GameId);
		Length when Length =:= 0 ->
			quit_game(GameId);
		_ ->
			{ok, pending}
	end.

start_game(GameId) ->
	case update_game_state(GameId, ?STARTED) of
		{ok, _} = Reply ->
			%% TODO propagate game started event
			{ok, started};
		{error, Reason} = Error ->
			%% TODO dispatch game error
			Error
	end.

quit_game(GameId) ->
	case update_game_state(GameId, ?QUIT) of
		{ok, _} = Reply ->
			%% TODO propagate game quit event
			{ok, quit};
		{error, Reason} = Error ->
			%% TODO dispatch game error
			Error
	end.

win_game(GameId, WinnerId) ->
	case mnesia:sync_transaction(fun() -> 
									[Game] = mnesia:wread(game, GameId),
									mnesia:write(Game#game{state = ?WON, 
														   winner_id = WinnerId, 
														   modified = now()})
								 end) of
		{atomic, Result} ->
			%% TODO dispatch game won
			{ok, won};
		{aborted, Reason} ->
			%% TODO dispatch game error
			{error, Reason}
	end.

update_game_state(GameId, GameState) ->
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

save_player(GameId, Handle) ->
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

in_players(GameId) ->
	PlIds = player_ids(GameId),
	MatchSpec = [{{'_', PlId, false, '_', '_', '_'}, [], ['$_']} || PlId <- PlIds],
	mnesia:select(player, MatchSpec).

update_out_player(PlayerId) ->
	case mnesia:sync_transaction(fun() -> 
										 [Player] = mnesia:wread(player, PlayerId),
										 mnesia:write(Player#player{is_out = true, modified = now()})
								 end) of
		{atomic, Result} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

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
