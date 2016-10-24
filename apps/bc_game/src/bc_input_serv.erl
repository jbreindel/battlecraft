
-module(bc_input_serv).
-behavior(gen_server).

%% api functions
-export([start_link/2,
		 create_player_serv/2,
		 spawn_player_bases/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 terminate/2]).

%% state rec
-record(state, {input_sup,
				entity_sups,
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

-spec spawn_player_bases(BcInputServ :: pid(), 
						 BcPlayers :: [bc_player:player()]) -> ok | 
															   {error, Reason :: string()}.
spawn_player_bases(BcInputServ, BcPlayers) ->
	gen_server:call(BcInputServ, {spawn_player_bases, BcPlayers}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init([BcInputSup, BcGame]) ->
	BcMap = bc_map:init(BcInputSup),
	{ok, #state{input_sup = BcInputSup,
				entity_sups = dict:new(),
		   		game = BcGame,
				map = BcMap,
				entities = undefined}}.

handle_call({create_player_serv, BcPlayer}, _From, 
	#state{input_sup = BcInputSup,
		   entity_sups = BcEntitySupDict,
		   game = BcGame,
		   map = BcMap} = State) ->
	{BcEntities, UpdatedState} = entities(State),
	PlayerId = bc_player:id(BcPlayer),
	{ok, BcPlayerSup} = supervisor:start_child(BcInputSup, #{
		id => PlayerId,
		start => {bc_player_sup, start_link, []},
		restart => permanent,
		type => supervisor,
		modules => [bc_player_sup]
	}),
	{ok, BcEntitySup} = supervisor:start_child(BcPlayerSup, #{
		id => entity_sup,
		start => {bc_entity_sup, start_link, []},
		restart => permanent,
		type => supervisor,
		modules => [bc_entity_sup]																  
	}),
	%% TODO monitor entity sups
	BcGoldFsm = start_gold_fsm(BcPlayerSup, BcGame, BcPlayer),
	{ok, BcPlayerServ} = supervisor:start_child(BcPlayerSup, #{
		id => player_serv,
		start => {bc_player_serv, start_link, [BcEntitySup, BcGame, BcPlayer, 
											   BcGoldFsm, BcMap, BcEntities]},
		restart => transient,
		shutdown => 1000,
		modules => [bc_player_serv]
	}),
	{reply, {ok, BcPlayerServ}, 
	 	UpdatedState#state{entity_sups = 
							   dict:store(PlayerId, BcEntitySup, BcEntitySupDict)}};

handle_call({spawn_player_bases, BcPlayers}, _From,
	#state{input_sup = BcInputSup,
		   entity_sups = BcEntitySupDict,
		   game = BcGame,
		   map = BcMap} = State) ->
	{BcEntities, UpdatedState} = entities(State),
	EntitiesEventPid = bc_entities:event(BcEntities),
	lists:foreach(fun(BcPlayer) -> 
					gen_event:add_handler(EntitiesEventPid, bc_entity_event, [BcPlayer]) 
				  end, BcPlayers),
	SortedBcPlayers = 
		lists:sort(fun(BcPlayer1, BcPlayer2) -> 
					 bc_player:team(BcPlayer1) < bc_player:team(BcPlayer2)
				   end, BcPlayers),
	BaseBcCollisions = 
		case SortedBcPlayers of
			Players when length(Players) == 2 ->
				[{1, bc_collision:init(uuid:get_v4(), bc_map:base1_vertices(BcMap))},
			 	 {3, bc_collision:init(uuid:get_v4(), bc_map:base3_vertices(BcMap))}];
			Players when length(Players) == 4 ->
				[{1, bc_collision:init(uuid:get_v4(), bc_map:base1_vertices(BcMap))},
			 	 {2, bc_collision:init(uuid:get_v4(), bc_map:base2_vertices(BcMap))},
			 	 {3, bc_collision:init(uuid:get_v4(), bc_map:base3_vertices(BcMap))},
			 	 {4, bc_collision:init(uuid:get_v4(), bc_map:base4_vertices(BcMap))}]
		end,
	lists:foldl(fun(BcPlayer, Num) -> 
					{_, BaseBcCollision} = lists:nth(Num, BaseBcCollisions),
					BaseUuid = bc_collision:uuid(BaseBcCollision),
					gen_event:add_handler(EntitiesEventPid, 
										  bc_base_event, 
										  [BaseUuid, BcGame, BcPlayer]), 
					Num + 1
				end, 1, SortedBcPlayers),
	Reply = spawn_player_bases(SortedBcPlayers, BaseBcCollisions, UpdatedState),
	{reply, Reply, UpdatedState}.

terminate(Reason, State) ->
	io:format("BcGameFsm terminates with ~p~n", [Reason]),
	ok.

%%====================================================================
%% Internal functions
%%====================================================================

start_gold_fsm(BcPlayerSup, BcGame, BcPlayer) ->
	{ok, BcGoldSup} = supervisor:start_child(BcPlayerSup, #{
		id => gold_sup,
		type => supervisor,
		start => {bc_gold_sup, start_link, []},
		restart => transient,
		shutdown => 100,
		modules => [bc_gold_sup]
	}),
	{ok, BcGoldFsm} = supervisor:start_child(BcGoldSup, #{
		id => gold_fsm,
		start => {bc_gold_fsm, start_link, [BcPlayer]},
		restart => transient,
		shutdown => 100,
		modules => [bc_gold_fsm]
	}),
	EventPid = bc_game:event(BcGame),
	gen_event:add_handler(EventPid, bc_gold_event, {gold_fsm, BcGoldFsm}),
	BcGoldFsm.

entities(#state{input_sup = BcInputSup,
				entities = BcEntities} = State) ->
	case BcEntities of
		undefined ->
			CreatedBcEntities = bc_entities:init(BcInputSup),
			{CreatedBcEntities, State#state{entities = CreatedBcEntities}};
		_ ->
			{BcEntities, State}
	end.

spawn_player_bases([], _, _) ->
	ok;
spawn_player_bases([BcPlayer|BcPlayers], [{PlayerNum, BaseBcCollision}|BaseBcCollisions], 
				   #state{input_sup = BcInputSup,
						  map = BcMap, 
						  entities = BcEntities,
						  entity_sups = BcEntitySupDict} = State) ->
	PlayerId = bc_player:id(BcPlayer),
	{ok, BaseBcEntityConfig} = bc_entities:entity_config(base, BcEntities),
	case dict:find(PlayerId, BcEntitySupDict) of
		{ok, BcEntitySup} ->
			bc_entity_util:spawn_entity(BaseBcCollision, BcPlayer, 
										BcEntitySup, BaseBcEntityConfig, 
										PlayerNum, BcMap, BcEntities),
			EntitiesEventPid = bc_entities:event(BcEntities),
			
			spawn_player_bases(BcPlayers, BaseBcCollisions, State);
		error ->
			{error, "Can't find entity supervisor."}
	end.
