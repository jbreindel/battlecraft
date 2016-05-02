
-module(bc_game_serv).
-behavior(gen_server).

%% api functions
-export([start_link/0,
		 create_game/1,
		 join_game/1,
		 get_game/1
		]).

%% gen_server callbacks
-export(init/1,
		handle_call/3,
		handle_cast/2).

%% state rec
-record(state, {
				sup,
				active_games
				}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

create_game(Privacy) ->
	gen_server:call(bc_game_serv, {create_game, Privacy}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

handle_call({create_game, Privacy}, _From, S = #state{sup = Sup, active_games = Games}) ->
	{reply, {ok, }}
