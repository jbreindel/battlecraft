
-module(bc_game_serv).
-behavior(gen_server).
-include("bc_game.hrl").

%% api functions
-export([start_link/2,
		 get_player/2,
		 get_all_players/1,
		 player_join/3,
		 player_quit/2
		]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_info/2
		]).

%% state rec
-record(state, {
				sup,
				game_id,
				players
				}).

%%====================================================================
%% API functions
%%====================================================================

start_link(GameId, BcGameSup) ->
	gen_server:start_link(?MODULE, {GameId, BcGameSup}, []).

get_player(GamePid, PlayerId) ->
	gen_server:call(GamePid, {get_player, PlayerId}).

get_all_players(GamePid) ->
	gen_server:call(GamePid, get_all_players).

player_join(GamePid, PlayerPid, Handle) ->
	gen_server:call(GamePid, {player_join, PlayerPid, Handle}).

player_quit(GamePid, PlayerId) ->
	gen_server:call(GamePid, {player_quit, PlayerId}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init({GameId, BcGameSup}) ->
	self() ! {start_game_fsm, GameId, BcGameSup},
	{ok, #state{sup = BcGameSup, game_id = GameId, players = dict:new()}}.

handle_call({get_player, PlayerId}, _From, State) ->
	Players = State#state.players,
	case dict:find(PlayerId, Players) of 
		{ok, PlayerPid} ->
			{reply, {ok, PlayerPid}, State};
		error ->
			{reply, {error, not_found}, State}
	end;

handle_call(get_all_players, _From, State) ->
	Players = State#state.players,
	{reply, {ok, dict:to_list(Players)}, State};

handle_call({player_join, PlayerPid, Handle}, _From, 
	State = #state{sup = BcGameSup, game_id = GameId, players = Players}) ->
	case save_player(GameId, Handle) of
		{ok, PlayerId} ->
			{reply, {ok, PlayerId}, State#state{players = 
													dict:store(PlayerId, PlayerPid, Players)}};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;

handle_call({player_quit, PlayerId}, _From, 
	State = #state{sup = BcGameSup, game_id = GameId, players = Players}) ->
	case update_out_player(GameId, PlayerId) of
		ok ->
			{reply, ok, State#state{players = dict:erase(PlayerId, Players)}};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end.
	
handle_info({start_game_fsm, GameId, BcGameSup}, State) ->
	{ok, Pid} = supervisor:start_child(BcGameSup, 
		#{id => GameId,
			start => {bc_game_fsm, start_link, [GameId]},
			restart => temporary,
			shutdown => 10000,
			type => worker,
			modules => [bc_game_fsm]}),
	%% TODO link pid
	{noreply, State}.

%%====================================================================
%% Internal functions
%%====================================================================

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

update_out_player(_, PlayerId) ->
	case mnesia:sync_transaction(fun() -> 
										 [Player] = mnesia:wread(player, PlayerId),
										 mnesia:write(Player#player{is_out = true, modified = now()})
								 end) of
		{atomic, Result} ->
			ok;
		{aborted, Reason} ->
			{error, Reason}
	end.

