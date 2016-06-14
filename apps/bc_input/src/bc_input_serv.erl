
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
				game_fsm,
				game_event,
				map_graph}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(BcInputSup :: pid(), 
				 BcGameFsm :: pid(), 
				 GameEventPid :: pid()) -> gen:start_ret().
start_link(BcInputSup, BcGameFsm, GameEventPid) ->
	gen_server:start_link(?MODULE, [BcInputSup, BcGameFsm, GameEventPid], []).

-spec create_player_serv(BcInputServ :: pid(), 
						 PlayerId :: integer(), 
						 PlayerPid :: pid()) -> {ok, Pid :: pid()} | {error, Reason :: string()}.
create_player_serv(BcInputServ, PlayerId, PlayerPid) ->
	gen_server:call(BcInputServ, {create_player_serv, PlayerId, PlayerPid}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init([BcInputSup, BcGameFsm, GameEventPid]) ->
	MapGraph = bc_map:init(BcInputSup),
	{ok, #state{input_sup = BcInputSup,
		   		game_fsm = BcGameFsm, 
				game_event = GameEventPid, 
				map_graph = MapGraph}}.

%% handle_call({create_player_serv, PlayerId, PlayerPid}, _From, 
%% 	#state{input_sup = BcInputSup,
%% 		   game_fsm = BcGameFsm, 
%% 		   game_event = GameEventPid, 
%% 		   map_graph = MapGraph} = State) ->
%% 	{ok, BcPlayerSup} = supervisor:start_child(BcInputSup, #{
%% 		id => PlayerId,
%% 		start => {bc_player_sup, start_link, []},
%% 		modules => [bc_player_sup]
%% 	}),
%% 	
	
