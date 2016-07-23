
-module(bc_input_serv).
-behavior(gen_server).

%% api functions
-export([start_link/2,
		 create_player_serv/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3]).

%% state rec
-record(state, {input_sup,
				game,
				map,
				entities}).

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
	BcMap = bc_map:init(BcInputSup),
	BcEntities = bc_entities:init(BcInputSup),
	{ok, #state{input_sup = BcInputSup,
		   		game = BcGame,
				map = BcMap,
				entities = BcEntities}}.

handle_call({create_player_serv, BcPlayer}, _From, 
	#state{input_sup = BcInputSup,
		   game = BcGame,
		   map = BcMap,
		   entities = BcEntities} = State) ->
	PlayerId = bc_player:id(BcPlayer),
	{ok, BcPlayerSup} = supervisor:start_child(BcInputSup, #{
		id => PlayerId,
		start => {bc_player_sup, start_link, []},
		restart => permanent,
		type => supervisor,
		modules => [bc_player_sup]
	}),
	BcGoldFsm = start_gold_fsm(BcPlayerSup, BcGame, BcPlayer),
	{ok, BcPlayerServ} = supervisor:start_child(BcPlayerSup, #{
		id => player_serv,
		start => {bc_player_serv, start_link, [BcPlayerSup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities]},
		modules => [bc_player_serv]
	}),
	{reply, {ok, BcPlayerServ}, State}.

%%====================================================================
%% Internal functions
%%====================================================================

start_gold_fsm(BcPlayerSup, BcGame, BcPlayer) ->
	{ok, BcGoldSup} = supervisor:start_child(BcPlayerSup, #{
		id => gold_sup,
		start => {bc_gold_sup, start_link, []},
		modules => [bc_gold_sup]
	}),
	{ok, BcGoldFsm} = supervisor:start_child(BcGoldSup, #{
		id => gold_fsm,
		start => {bc_gold_fsm, start_link, [BcPlayer]},
		modules => [bc_gold_fsm]
	}),
	EventPid = bc_game:event(BcGame),
	gen_event:add_handler(EventPid, bc_gold_event, {gold_fsm, BcGoldFsm}),
	BcGoldFsm.
