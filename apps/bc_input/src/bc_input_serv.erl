
-module(bc_input_serv).
-behavior(gen_server).

%% api functions
-export([start_link/1,
		 create_player_serv/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/3]).

%% state rec
-record(state, {input_sup,
				game,
				map_graph}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(BcInputSup :: pid(), 
				 BcGame :: bc_game:game()) -> gen:start_ret().
start_link(BcInputSup, BcGame) ->
	gen_server:start_link(?MODULE, [BcInputSup, BcGame], []).

-spec create_player_serv(BcInputServ :: pid(), 
						 BcPlayer :: bc_player:player()) -> {ok, Pid :: pid()} | 
															{error, Reason :: string()}.
create_player_serv(BcInputServ, BcPlayer) ->
	gen_server:call(BcInputServ, {create_player_serv, BcPlayer}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init([BcInputSup, BcGame]) ->
	MapGraph = bc_map:init(BcInputSup),
	{ok, #state{input_sup = BcInputSup,
		   		game = BcGame,
				map_graph = MapGraph}}.

handle_call({create_player_serv, BcPlayer}, _From, 
	#state{input_sup = BcInputSup,
		   game = BcGame,
		   map_graph = MapGraph} = State) ->
	{ok, BcPlayerSup} = supervisor:start_child(BcInputSup, #{
		id => PlayerId,
		start => {bc_player_sup, start_link, []},
		restart => permanent,
		type => supervisor,
		modules => [bc_player_sup]
	}),
	{ok, BcPlayerServ} = supervisor:start_child(BcPlayerSup, #{
		id => player_serv
		start => {bc_player_serv, start_link, [BcPlayerSup, BcGame, BcPlayer, MapGraph]},
		modules => [bc_player_serv]
	}).
	{reply, {ok, BcPlayerServ}, State}.
