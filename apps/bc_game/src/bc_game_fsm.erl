
-module(bc_game_fsm).
-behavior(gen_fsm).
-include("../include/bc_game_state.hrl").

%% number of players to start game
-define(MAX_PLAYERS, 1).

%% exported funcs
-export([start_link/4, 
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
-record(state, {input_serv,
				game,
				team,
				players}).

%%====================================================================
%% Public functions
%%====================================================================

-spec start_link(GameId :: integer(), 
				 GameEventPid :: pid(), 
				 BcInputSup :: pid(), 
				 BcEntities :: bc_entities:entities()) -> gen:start_ret().
start_link(GameId, GameEventPid, BcInputSup, BcEntities) ->
	gen_fsm:start_link(?MODULE, [GameId, GameEventPid, BcInputSup, BcEntities], []).

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

init([GameId, GameEventPid, BcInputSup, BcEntities]) ->
	BcGame = bc_game:create(GameId, GameEventPid, self()),
	{ok, BcInputServ} = supervisor:start_child(BcInputSup, #{
		id => bc_input_serv,
		start => {bc_input_serv, start_link, [BcInputSup, BcGame, BcEntities]},
		modules => [bc_input_serv]
	}),
	{ok, pending, #state{input_serv = BcInputServ,
						 game = BcGame,
						 team = 1,
						 players = dict:new()}}.

pending({player_join, #{player_pid := PlayerPid, 
						handle := Handle}},
			_From, #state{input_serv = BcInputServ,
				   		  game = BcGame,
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
			UpdatedPlayers = dict:store(PlayerId, #{player => BcPlayer,
										  			monitor => erlang:monitor(process, PlayerPid)}, Players),
			UpdateTeam = toggle_team(Team),
			UpdatedState = State#state{team = UpdateTeam, players = UpdatedPlayers},
			{ok, BcPlayerServ} = bc_input_serv:create_player_serv(BcInputServ, BcPlayer),
			case pending_players_changed(GameId, UpdatedPlayers) of
				{ok, started} ->
					BcPlayers = lists:map(fun({_, V}) -> 
											maps:get(player, V) 
										  end, dict:to_list(UpdatedPlayers)),
					bc_input_serv:spawn_player_bases(BcInputServ, BcPlayers),
					gen_event:notify(GameEventPid, {game_started, BcPlayers}),
					{reply, {ok, PlayerId, Team, BcPlayerServ}, started, UpdatedState};
				{ok, pending} ->
					{reply, {ok, PlayerId, Team, BcPlayerServ}, pending, UpdatedState};
				{error, Reason} = Error ->
					io:format("Error: ~p~n", [Error]),
					gen_event:notify(GameEventPid, {game_error, Reason}),
					gen_event:stop(GameEventPid),
					{stop, Error, Error, UpdatedState};
				_ ->
					{stop, illegal_state, {error, illegal_state}, UpdatedState}
			end;
		{error, Reason} = Error ->
			io:format("Error: ~p~n", [Error]),
			{reply, Error, pending, State}
	end.

pending({player_quit, PlayerId},
			#state{input_serv = BcInputServ,
				   game = BcGame,
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
			#state{input_serv = BcInputServ,
				   game = BcGame,
				   players = Players} = State) ->
	case bc_player_model:update_out(OutPlayerId, true) of
		ok ->
			GameEventPid = bc_game:event(BcGame),
			#{player := BcPlayer} = dict:fetch(OutPlayerId, Players),
			gen_event:notify(GameEventPid,
							 {player_out, BcPlayer}),
			GameId = bc_game:id(BcGame),
			InPlayerIds = bc_player_model:in_player_ids(GameId),
			case length(InPlayerIds) of
				Length when Length =:= 1 ->
					[InPlayerId] = InPlayerIds,
					case bc_game_model:win(GameId, InPlayerId) of
						{ok, won} ->
							gen_event:notify(GameEventPid,
											{game_won, BcPlayer}),
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

handle_info({'DOWN', Ref, process, Pid, _}, StateName,
		#state{input_serv = BcInputServ,
			   game = BcGame,
			   players = Players} = State) ->
	case lists:filter(fun({K, #{monitor := Monitor}}) -> 
					  	Monitor =:= Ref
				 	  end, dict:to_list(Players)) of
		[{PlayerId, _}] ->
			started({player_quit, PlayerId}, State);
		_ ->
			ok
	end.

terminate(Reason, StateName, State) ->
	io:format("BcGameFsm terminates with ~p~n", [Reason]).

%%====================================================================
%% Internal functions
%%====================================================================

toggle_team(Team) ->
	case Team of
		1 -> 2;
		2 -> 1
	end.

pending_players_changed(GameId, Players) ->
	case dict:size(Players) of
		Length when Length =:= ?MAX_PLAYERS ->
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

find_player_id(PlayerPid, Players) ->
	List = dict:to_list(Players),
	[{Id, Pid}] = lists:filter(fun({_, Pid} = Tuple) when Pid =:= PlayerPid -> Tuple end, List),
	Id.
