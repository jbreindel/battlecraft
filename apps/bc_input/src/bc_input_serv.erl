
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
	{reply, {ok, BcPlayerServ}, State};

handle_call({spawn_player_bases, BcPlayers}, _From,
	#state{input_sup = BcInputSup,
		   game = BcGame,
		   map = BcMap,
		   entities = BcEntities} = State) ->
	SortedBcPlayers = lists:sort(fun(BcPlayer1, BcPlayer2) -> 
									bc_player:team(BcPlayer1) < bc_player:team(BcPlayer2) 
								 end, BcPlayers),
	BaseBcCollisions = [bc_collision:init(uuid:get_v4(), bc_map:base1_vertices(BcMap)),
			 	 		bc_collision:init(uuid:get_v4(), bc_map:base2_vertices(BcMap)),
			 	 		bc_collision:init(uuid:get_v4(), bc_map:base3_vertices(BcMap)),
			 	 		bc_collision:init(uuid:get_v4(), bc_map:base4_vertices(BcMap))],
	{reply, spawn_player_bases(SortedBcPlayers, State, BaseBcCollisions), State}.

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

insert_base_entity(BcPlayer, BaseUuid, BaseBcVertices, BcEntities) ->
	{ok, BaseBcEntityConfig} = bc_entities:entity_config(base),
	%% TODO spawn ai impl
	PlayerId = bc_player:id(BcPlayer),
	Team = bc_player:team(BcPlayer),
	Health = bc_entity_config:health(BaseBcEntityConfig),
	BaseBcEntity = bc_entity:init(BaseUuid, PlayerId, Team, base, Health, BaseBcVertices),
	case bc_entities:insert_new(BaseBcEntity, BcEntities) of
		true ->
			EntitiesEventPid = bc_entities:event(BcEntities),
			gen_event:notify(EntitiesEventPid, {entity_spawned, BaseBcEntity}),
			true;
		false ->
			false
	end.

spawn_player_bases([], _, _) ->
	ok;
spawn_player_bases([BcPlayer|BcPlayers], 
				  #state{map = BcMap, 
						 entities = BcEntities} = State, [BaseBcCollision|BaseBcCollisions]) ->
	case bc_map:insert_collision(BaseBcCollision) of
		true ->
			BaseUuid = bc_collision:uuid(BaseBcCollision),
			BaseBcVertices = bc_collision:vertices(BaseBcCollision),
			insert_base_entity(BcPlayer, BaseUuid, BaseBcVertices, BcEntities),
			spawn_player_bases(BcPlayer, State, BaseBcCollisions);
		false ->
			{error, "Could not spawn base for player " ++ bc_player:id(BcPlayer)}
	end.
