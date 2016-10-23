
-module(bc_game_fsm).
-behavior(gen_fsm).
-include("../include/bc_game_state.hrl").

%% number of players to start game
-define(TIMEOUT, 30000).

%% exported funcs
-export([start_link/5, 
		 player_join/3, 
		 player_quit/2, 
		 player_out/2]).

-export([init/1, 
		 pending/2, 
		 pending/3, 
		 started/2, 
		 terminate/3, 
		 handle_info/3]).

%% state rec
-record(state, {game,
				game_sup,
				input_serv,
				team,
				players}).

%%====================================================================
%% Public functions
%%====================================================================

-spec start_link(GameId :: integer(), 
				 Privacy :: integer(),
				 MaxPlayers :: integer(),
				 GameEventPid :: pid(), 
				 BcGameSup :: pid()) -> gen:start_ret().
start_link(GameId, Privacy, MaxPlayers, GameEventPid, BcGameSup) ->
	gen_fsm:start_link(?MODULE, [GameId, Privacy, MaxPlayers, 
								 GameEventPid, BcGameSup], []).

-spec player_join(BcGameFsm :: pid(), 
				  PlayerPid :: pid(), 
				  Handle :: string()) -> {ok, 
										  PlayerId :: integer(), 
										  Team :: integer(), 
										  BcPlayerServ :: pid()} | 
											 {error, 
											  Reason :: string()}.
player_join(BcGameFsm, PlayerPid, Handle) ->
	gen_fsm:sync_send_event(BcGameFsm, 
							{player_join, #{player_pid => PlayerPid,
											handle => Handle}}).

-spec player_quit(BcGameFsm :: pid(), PlayerId :: integer()) -> ok.
player_quit(BcGameFsm, PlayerId) ->
	gen_fsm:send_event(BcGameFsm, {player_quit, PlayerId}).

-spec player_out(BcGameFsm :: pid(), PlayerId :: integer()) -> ok.
player_out(BcGameFsm, PlayerId) ->
	gen_fsm:send_event(BcGameFsm, {player_out, PlayerId}).

%%====================================================================
%% Gen_fsm callbacks 
%%====================================================================

init([GameId, Privacy, MaxPlayers, GameEventPid, BcGameSup]) ->
	BcGame = bc_game:init(GameId, Privacy, MaxPlayers, GameEventPid, self()),
	{ok, pending, #state{game = BcGame,
						 game_sup = BcGameSup,
						 input_serv = undefined,
						 team = 1,
						 players = dict:new()}, ?TIMEOUT}.

pending({player_join, #{player_pid := PlayerPid, 
						handle := Handle}},
			_From, #state{game = BcGame,
						  team = Team,
				   		  players = Players} = State) ->
	GameId = bc_game:id(BcGame),
	case bc_player_model:save(GameId, Handle, Team) of
		{ok, PlayerId} ->
			BcPlayer = bc_player:create(PlayerId, Handle, Team, PlayerPid),
			GameEventPid = bc_game:event(BcGame),
			gen_event:notify(GameEventPid, {player_joined, BcPlayer}),
			gen_event:add_handler(GameEventPid, {bc_game_event, PlayerId},
								  				{player, BcPlayer}),
			UpdatedPlayers = 
				dict:store(PlayerId, #{player => BcPlayer,
									   monitor => erlang:monitor(process, PlayerPid)}, Players),
			UpdateTeam = toggle_team(Team),
			{BcInputServ, UpdatedState} = 
				input_serv(State#state{team = UpdateTeam, players = UpdatedPlayers}),
			{ok, BcPlayerServ} = bc_input_serv:create_player_serv(BcInputServ, BcPlayer),
			case pending_players_changed(BcGame, UpdatedPlayers) of
				{ok, started} ->
					BcPlayers = 
						lists:map(
							fun({_, V}) -> 
								maps:get(player, V) 
							end, dict:to_list(UpdatedPlayers)),
					bc_input_serv:spawn_player_bases(BcInputServ, BcPlayers),
					gen_event:notify(GameEventPid, {game_started, BcPlayers}),
					{reply, {ok, PlayerId, Team, BcPlayerServ}, started, UpdatedState};
				{ok, pending} ->
					{reply, {ok, PlayerId, Team, BcPlayerServ}, pending, UpdatedState};
				{error, Reason} = Error ->
					gen_event:notify(GameEventPid, {game_error, Reason}),
					gen_event:stop(GameEventPid),
					{stop, Error, UpdatedState};
				_ ->
					{stop, illegal_state, {error, illegal_state}, UpdatedState}
			end;
		{error, Reason} = Error ->
			{reply, Error, pending, State}
	end.

pending(timeout, #state{game = BcGame} = State) ->
	GameId = bc_game:id(BcGame),
	case quit_game(GameId) of
		{ok, quit} ->
			{stop, normal, State};
		{error, Reason} = Error ->
			{stop, Error, State}
	end;
pending({player_quit, PlayerId},
			#state{game = BcGame,
				   team = Team,
				   players = Players} = State) ->
	GameId = bc_game:id(BcGame),
	case bc_player_model:delete(GameId, PlayerId) of
		ok ->
			GameEventPid = bc_game:pid(BcGame),
			gen_event:delete_handler(GameEventPid, {bc_game_event, PlayerId}, []),
			#{player := BcPlayer, monitor := Monitor} = dict:fetch(PlayerId, Players),
			gen_event:notify(GameEventPid, {player_quit, BcPlayer}),
			erlang:demonitor(Monitor),
			UpdatedPlayers = dict:erase(PlayerId, Players),
			UpdatedState = State#state{team = toggle_team(Team), players = UpdatedPlayers},
			case pending_players_changed(BcGame, UpdatedPlayers) of
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
			#state{game = BcGame,
				   players = Players} = State) ->
	case bc_player_model:update_out(OutPlayerId, true) of
		ok ->
			GameEventPid = bc_game:event(BcGame),
			#{player := BcPlayer} = dict:fetch(OutPlayerId, Players),
			gen_event:notify(GameEventPid, {player_out, BcPlayer}),
			GameId = bc_game:id(BcGame),
			InPlayerIds = bc_player_model:in_player_ids(GameId),
			WinThreashold = bc_game:max_players(BcGame) / 2,
			case find_players(InPlayerIds, Players) of
				InBcPlayers when length(InBcPlayers) =< WinThreashold ->
					FirstBcPlayer = lists:nth(1, InBcPlayers),
					WinningTeam = bc_player:team(FirstBcPlayer),
					case bc_game_model:win(GameId, WinningTeam, ?WON) of
						{ok, won} ->
							gen_event:notify(GameEventPid, {game_won, InBcPlayers}),
							gen_event:stop(GameEventPid),
							{stop, normal, State};
						{error, Reason} = Error ->
							gen_event:notify(GameEventPid, {game_error, Reason}),
							gen_event:stop(GameEventPid),
							{stop, Error, State}
					end;
				_ ->
					%% TODO quit game?
					{next_state, started, State}
			end;
		{error, Reason} = Error ->
			{stop, Error, State}
	end.

handle_info({'DOWN', Ref, process, Pid, _}, StateName,
		#state{game = BcGame,
			   players = Players} = State) ->
	case lists:filter(
		 	fun({K, #{monitor := Monitor}}) -> 
				Monitor =:= Ref
			end, dict:to_list(Players)) of
		[{PlayerId, _}] ->
			case StateName of
				started ->
					started({player_quit, PlayerId}, State);
				pending ->
					pending({player_quit, PlayerId}, State);
				_ ->
					{stop, {error, "Incompatible State", State}}
			end;
		_ ->
			ok
	end.

terminate(Reason, StateName, State) ->
	io:format("BcGameFsm terminates with ~p~n", [Reason]),
	ok.

%%====================================================================
%% Internal functions
%%====================================================================

toggle_team(Team) ->
	case Team of
		1 -> 2;
		2 -> 1
	end.

pending_players_changed(BcGame, Players) ->
	GameId = bc_game:id(BcGame),
	MaxPlayers = bc_game:max_players(BcGame),
	case dict:size(Players) of
		Length when Length =:= MaxPlayers ->
			start_game(GameId);
		Length when Length =:= 0 ->
			quit_game(GameId);
		_ ->
			{ok, pending}
	end.

start_game(GameId) ->
	case bc_game_model:update_state(GameId, ?STARTED) of
		{ok, _} = Reply ->
			{ok, started};
		{error, Reason} = Error ->
			Error
	end.

quit_game(GameId) ->
	case bc_game_model:update_state(GameId, ?QUIT) of
		{ok, _} = Reply ->
			{ok, quit};
		{error, Reason} = Error ->
			Error
	end.

input_serv(#state{game = BcGame,
				  game_sup = BcGameSup, 
				  input_serv = BcInputServ} = State) ->
	case BcInputServ of
		undefined ->
			{ok, BcInputSup} = supervisor:start_child(BcGameSup, #{
				id => bc_input_sup,
				type => supervisor,
				start => {bc_input_sup, start_link, []},
				restart => transient,
				shutdown => 1000,
				modules => [bc_input_sup]
			}),	
			{ok, StartedBcInputServ} = supervisor:start_child(BcInputSup, #{
				id => bc_input_serv,
				start => {bc_input_serv, start_link, [BcInputSup, BcGame]},
				restart => transient,
				shutdown => 1000,
				modules => [bc_input_serv]
			}),
			{StartedBcInputServ, State#state{input_serv = StartedBcInputServ}};
		_ ->
			{BcInputServ, State}
	end.

find_player_id(PlayerPid, Players) ->
	List = dict:to_list(Players),
	[{Id, Pid}] = 
		lists:filter(
			fun({_, Pid} = Tuple) 
				 when Pid =:= PlayerPid -> 
				Tuple 
			end, List),
	Id.

find_players(PlayerIds, PlayerDict) ->
	lists:filtermap(
		fun(PlayerId) -> 
			case dict:find(PlayerId, PlayerDict) of 
				{ok, PlayerMap} -> 
					BcPlayer = maps:get(player, PlayerMap),
					{true, BcPlayer};
				error -> 
					false 
			end 
		end, PlayerIds).
