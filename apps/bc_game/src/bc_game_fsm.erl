
-module(bc_game_fsm).
-behavior(gen_fsm).
-include("../include/bc_game_state.hrl").
-include("bc_game.hrl").

%% exported funcs
-export([start_link/2, player_join/3, player_quit/2, player_out/2]).
-export([init/2, pending/2, started/2]).

%% state rec
-record(state, {game_sup,
				input_sup,
				gold_sup,
				entity_sup,
				game_event,
				game_id,
				map_graph,
				collision_tab,
				players}).

%%====================================================================
%% Public functions
%%====================================================================

start_link(GameId, BcGameSup) ->
	gen_fsm:start_link(?MODULE, [GameId, BcGameSup], []).

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

init(GameId, BcGameSup) ->
	{ok, BcInputSup} = supervisor:start_child(BcGameSup, #{
		id => bc_input_sup,
		start => {bc_input_serv, start_link, []},
		modules => [bc_input_serv]													   
	}),
	{ok, BcGoldSup} = supervisor:start_child(BcGameSup, #{
		id => bc_gold_sup,
		start => {bc_gold_sup, start_link, []},
		modules => [bc_gold_sup]													  
	}),
	{ok, BcEntitySup} = supervisor:start_child(BcGameSup, #{
		id => bc_entity_sup,
		start => {bc_entity_sup, start_link, []},
		modules => [bc_entity_sup]
	}),
	{ok, GameEventPid} = supervisor:start_child(BcGameSup, #{
		id => bc_game_event,
		start => {gen_event, start_link, []},
		modules => [gen_event]
	}),
	MapGraph = bc_map:init(),
	CollisionTab = bc_collision:init(BcGameSup),
	{ok, pending, #state{game_sup = BcGameSup,
						 input_sup = BcInputSup,
						 gold_sup = BcGoldSup,
						 entity_sup = BcEntitySup,
						 game_event = GameEventPid,
						 game_id = GameId,
						 map_graph = MapGraph,
						 collision_tab = CollisionTab,
						 players = dict:new()}}.

pending({player_join, #{player_pid := PlayerPid, 
						handle := Handle}},
			#state{game_sup = BcGameSup,
				   input_sup = BcInputSup,
				   gold_sup = BcGoldSup,
				   entity_sup = BcEntitySup,
				   game_event = GameEventPid,
				   game_id = GameId,
				   map_graph = MapGraph,
				   collision_tab = CollisionTab,
				   players = Players} = State) ->
	case save_player(GameId, Handle) of
		{ok, PlayerId} ->
			gen_event:notify(GameEventPid, {player_joined, PlayerId, Handle}),
			gen_event:add_handler(GameEventPid,
								  {bc_game_event, PlayerId},
								  {player_pid, PlayerPid}),
			{ok, BcGoldFsm} = supervisor:start_child(BcGoldSup, #{
				id => PlayerId,
				start => {bc_gold_fsm, start_link, [PlayerId, PlayerPid]},
				modules => [bc_gold_fsm]													  
			}),
			{ok, BcInputServ} = supervisor:start_child(BcInputSup, #{
				id => PlayerId,
				start => {bc_input_serv, start_link, [BcEntitySup, MapGraph, CollisionTab, BcGoldFsm]},
				modules => [bc_input_serv]							  
			}),
			gen_event:add_handler(GameEventPid, 
								  {bc_gold_event, PlayerId ++ "-gold"}, {gold_fsm, BcGoldFsm}),
			UpdatedPlayers = dict:store(PlayerId,
										#{pid => PlayerPid,
										  monitor => erlang:monitor(process, PlayerPid)}, Players),
			UpdatedState = State#state{players = UpdatedPlayers},
			case pending_players_changed(GameId, UpdatedPlayers) of
				{ok, started} ->
					%%gen_event:notify(GameEventPid, {game_started, dict:to_list(UpdatedPlayers)}),
					{reply, {ok, PlayerId, BcInputServ}, started, UpdatedState};
				{ok, pending} ->
					{reply, {ok, PlayerId, BcInputServ}, pending, UpdatedState};
				{error, Reason} = Error ->
					gen_event:notify(GameEventPid, {game_error, Reason}),
					gen_event:stop(GameEventPid),
					{stop, Error, Error, UpdatedState};
				_ ->
					{stop, illegal_state, {error, illegal_state}, UpdatedState}
			end;
		{error, Reason} = Error ->
			{reply, Error, pending, State}
	end;
pending({player_quit, PlayerId},
			#state{game_event = GameEventPid,
				   game_id = GameId,
				   players = Players} = State) ->
	case remove_player(GameId, PlayerId) of
		ok ->
			QuitPlayer = find_player(PlayerId),
			gen_event:delete_handler(GameEventPid, {bc_game_event, PlayerId}, []),
			gen_event:notify(GameEventPid, {player_quit, PlayerId, QuitPlayer#player.handle}),
			case dict:find(PlayerId, Players) of
				{ok, #{monitor := Monitor}} ->
					erlang:demonitor(Monitor);
				_ ->
					ok
			end,
			UpdatedPlayers = dict:erase(PlayerId, Players),
			UpdatedState = State#state{players = UpdatedPlayers},
			case pending_players_changed(GameId, UpdatedPlayers) of
				{ok, quit} ->
					gen_event:stop(GameEventPid),
					{stop, quit, UpdatedState};
				{ok, pending} ->
					{next_state, pending, UpdatedState};
				{error, Reason} = Error ->
					gen_event:notify(GameEventPid, {game_error, Reason}),
					gen_event:stop(GameEventPid),
					{stop, Error, UpdatedState};
				_ ->
					{stop, illegal_state, UpdatedState}
			end;
		{error, Reason} = Error ->
			{stop, Error, State}
	end.

started({_, OutPlayerId}, 
		State = #state{game_sup = BcGameSup,
					   game_event = GameEventPid,
					   game_id = GameId,
					   players = Players}) ->
	case update_out_player(OutPlayerId) of
		ok ->
			OutPlayer = find_player(OutPlayerId),
			gen_event:notify(GameEventPid,
							 {player_out, OutPlayerId, OutPlayer#player.handle}),
			InPlayers = in_players(GameId),
			case length(InPlayers) of
				Length when Length =:= 1 ->
					[InPlayer] = InPlayers,
					case win_game(GameId, InPlayer#player.id) of
						{ok, won} ->
							gen_event:notify(GameEventPid, 
											{game_won, InPlayer#player.id, InPlayer#player.handle}),
							gen_event:stop(GameEventPid),
							{stop, won, State};
						{error, Reason} = Error ->
							gen_event:notify(GameEventPid, {game_error, Reason}),
							gen_event:stop(GameEventPid),
							{stop, Error, State}
					end;
				_ ->
					{next_state, started, State}
			end;
		{error, Reason} = Error ->
			{stop, Error, State}
	end.

handle_info({'DOWN', Ref, process, Pid, _}, State) ->
	PlayerId = find_player_id(Pid, State#state.players),
	started({player_quit, PlayerId}, State).

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
			{ok, started};
		{error, Reason} = Error ->
			Error
	end.

quit_game(GameId) ->
	case update_game_state(GameId, ?QUIT) of
		{ok, _} = Reply ->
			{ok, quit};
		{error, Reason} = Error ->
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
			{ok, won};
		{aborted, Reason} ->
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

find_player_id(PlayerPid, Players) ->
	List = dict:to_list(Players),
	[{Id, Pid}] = lists:filter(fun({_, Pid} = Tuple) when Pid =:= PlayerPid -> Tuple end, List),
	Id.

find_player(PlayerId) ->
	[Player] = mnesia:wread(player, PlayerId),
	Player.

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
