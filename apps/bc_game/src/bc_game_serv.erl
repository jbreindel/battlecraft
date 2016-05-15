
-module(bc_game_serv).
-behavior(gen_server).

%% api functions
-export([start_link/0,
		 create_game/1,
		 get_game/1,
		 get_player/2,
		 get_all_players/1,
		 add_player/3,
		 remove_player/2,
		 remove_game/1
		]).

%% gen_server callbacks
-export(init/1,
		handle_call/3,
		handle_cast/2).

%% state rec
-record(state, {
				sup,
				games
				}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_game(Privacy) ->
	gen_server:call(?MODULE, {create_game, Privacy}).

get_game(GameId) ->
	gen_server:call(?MODULE, {get_game, GameId}).

get_player(GameId, PlayerId) ->
	gen_server:call(?MODULE, {get_player, GameId, PlayerId}).

get_all_players(GameId) ->
	gen_server:call(?MODULE, {get_all_players, GameId}).

add_player(GameId, PlayerId, PlayerPid) ->
	gen_server:call(?MODULE, {add_player, PlayerId, PlayerPid}).

remove_player(GameId, PlayerId) ->
	gen_server:call(?MODULE, {remove_player, PlayerId}).

remove_game(GameId) ->
	gen_server:call(?MODULE, {remove_game, GameId}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

handle_call({create_game, Privacy}, _From, S = #state{sup = Sup, active_games = Games}) ->
	{reply, {ok, }}
