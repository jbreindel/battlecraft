
-module(bc_game_fsm).
-behavior(gen_fsm).
-include("../include/bc_game_state.hrl").
-include("bc_game.hrl").

%% exported funcs
-export([start_link/1, player_join/3, player_quit/2, player_out/2]).
-export([init/1, created/2, pending/2, started/2]).

%% state rec
-record(state, #{game_id}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(GameId) ->
	gen_fsm:start_link(?MODULE, [GameId], []).

player_join(GameFsmPid, PlayerId) ->
	gen_fsm:sync_send_event(GameFsmPid, {player_join, #{player_id => PlayerId}}).

player_quit(GameFsmPid, PlayerId) ->
	gen_fsm:send_event(GameFsmPid, {player_quit, #{player_id => PlayerId}}).

player_out(GameFsmPid, PlayerId) ->
	gen_fsm:send_event(GameFsmPid, {player_out, #{player_id => PlayerId}}).

%%====================================================================
%% Gen_fsm callbacks
%%====================================================================

init(GameId) ->
	{ok, created, #state{game_id => GameId}}.

created({player_join, #{player_id => PlayerId}}, State) ->
	{ok, pending, S}.

pending({_, #{player_id => PlayerId}},
		S = State#state{game_id = GameId}) ->
	InPlayers = in_players(GameId),
	case length(InPlayers) of
		Length when Length =:= 4 ->
			start_game(GameId);
		Length when Length =:= 0 ->
			quit_game(GameId);
		_ ->
			{next_state, pending, S}
	end.

started({_, OutPlayerId}, 
		S = State#state{game_id = GameId}) ->
	InPlayers = in_players(GameId),
	case length(InPlayers) of
		Length when Length =:= 1 ->
			[InPlayer] = InPlayers,
			case win_game(GameId, InPlayer#player.id) of
				ok ->
					%% TODO dispatch game won
					{stop, ok, S};
				{error, Reason} ->
					%% TODO dispatch game error
					{stop, {error, Reason}, S}
			end;
		_ ->
			{next_state, started, S}
	end;

%%====================================================================
%% Internal functions
%%====================================================================

start_game(GameId) ->
	case update_game_state(GameId, ?STARTED) of
		{ok, _} ->
			%% TODO propagate game started event
			{next_state, started, #state{game_id = GameId}};
		{error, Reason} ->
			%% TODO dispatch game error
			{stop, {error, Reason}, #state{game_id = GameId}}
	end.

quit_game(GameId) ->
	case update_game_state(GameId, ?QUIT) of
		{ok, _} ->
			%% TODO remove from active games
			{stop, ok, #state{game_id = GameId}};
		{error, Reason} ->
			%% TODO dispatch game error
			{stop, {error, Reason}, #state{game_id = GameId}}
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

game_player_ids(GameId) ->
	MatchSpec = ets:fun2ms(fun({_, _, GmId, _} = GpAssoc) 
								when GmId =:= GameId -> GpAssoc end),
	ResultList = mnesia:select(gp_assoc, MatchSpec),
	lists:map(fun(GpAssoc) -> GpAssoc#gp_assoc.player_id end, ResultList).

in_players(GameId) ->
	PlIds = game_player_ids(GameId),
	MatchSpec = [{{'_', PlId, false, '_', '_', '_'}, [], ['$_']} || PlId <- PlIds],
	mnesia:select(player, MatchSpec).

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

%% save_player(GameId, Handle) ->
%% 	Now = now(),
%% 	PlayerId = bc_model:gen_id(),
%% 	Player = player#{player_id = PlayerId,
%% 					 handle = Handle,
%% 					 is_out = false,
%% 					 created = Now,
%% 					 modified = Now},
%% 	GamePlayerAssoc = gp_assoc#{id = bc_model:gen_id(),
%% 								gp = {GameId, PlayerId}},
%% 	case mnesia:sync_transaction(fun() ->
%% 										 mnesia:write(Player),
%% 										 mnesia:write(GamePlayerAssoc)
%% 								 end) of
%% 		{atomic, Result} ->
%% 			{ok, PlayerId};
%% 		{aborted, Reason} ->
%% 			{error, Reason}
%% 	end.
%% 
%% update_out_player(GameId, PlayerId) ->
%% 	case mnesia:sync_transaction(fun() -> 
%% 										 [Player] = mnesia:wread(player, PlayerId),
%% 										 mnesia:write(Player#player{is_out = true, modified = now()})
%% 								 end) of
%% 		{atomic, Result} ->
%% 			ok;
%% 		{aborted, Reason} ->
%% 			{error, Reason}
%% 	end.
%% 		
%% remove_player(GameId, PlayerId) ->
%% 	MatchSpec = ets:fun2ms(fun({_, _, GmId, PlId} = GP) 
%% 							  when GmId =:= GameId andalso 
%% 									   PlId =:= PlayerId -> GP end),
%% 	case mnesia:sync_transaction(fun() ->
%% 										case mnesia:select(gp_assoc, MatchSpec, 1, read) of
%% 											{[GPAssoc], 1} ->
%% 												Id = GPAssoc#gp_assoc.id,
%% 												mnesia:delete(gp_assoc, Id, write);
%% 											'$end_of_table' ->
%% 												ok
%% 										end,
%% 										mnesia:delete(player, PlayerId, write)
%% 								 end) of
%% 		{atomic, _} ->
%% 			ok;
%% 		{aborted, Reason} ->
%% 			{error, Reason}
%% 	end.
