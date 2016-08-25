
-module(bc_input_serv).
-behavior(gen_server).

%% api functions
-export([start_link/3,
		 create_player_serv/2,
		 spawn_player_bases/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 terminate/2]).

%% state rec
-record(state, {input_sup,
				game,
				map,
				entities}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link(BcInputSup :: pid(), 
				 BcGame :: bc_game:game(),
				 BcEntities :: bc_entities:entities()) -> gen:start_ret().
start_link(BcInputSup, BcGame, BcEntities) ->
	gen_server:start_link(?MODULE, [BcInputSup, BcGame, BcEntities], []).

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

init([BcInputSup, BcGame, BcEntities]) ->
	BcMap = bc_map:init(BcInputSup),
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
		start => {bc_player_serv, start_link, [BcPlayerSup, BcGame, BcPlayer, 
											   BcGoldFsm, BcMap, BcEntities]},
		modules => [bc_player_serv]
	}),
	{reply, {ok, BcPlayerServ}, State};

handle_call({spawn_player_bases, BcPlayers}, _From,
	#state{input_sup = BcInputSup,
		   game = BcGame,
		   map = BcMap,
		   entities = BcEntities} = State) ->
	EntitiesEventPid = bc_entities:event(BcEntities),
	lists:foreach(fun(BcPlayer) -> 
					gen_event:add_handler(EntitiesEventPid, bc_entity_event, [BcPlayer]) 
				  end, BcPlayers),
	SortedBcPlayers = lists:sort(fun(BcPlayer1, BcPlayer2) -> 
									bc_player:team(BcPlayer1) < bc_player:team(BcPlayer2) 
								 end, BcPlayers),
	BaseBcCollisions = [bc_collision:init(uuid:get_v4(), bc_map:base1_vertices(BcMap)),
			 	 		bc_collision:init(uuid:get_v4(), bc_map:base2_vertices(BcMap)),
			 	 		bc_collision:init(uuid:get_v4(), bc_map:base3_vertices(BcMap)),
			 	 		bc_collision:init(uuid:get_v4(), bc_map:base4_vertices(BcMap))],
	lists:foldl(fun(BcPlayer, Num) -> 
					BaseBcCollision = lists:nth(Num, BaseBcCollisions),
					BaseUuid = bc_collision:uuid(BaseBcCollision),
					gen_event:add_handler(EntitiesEventPid, bc_base_event, [BaseUuid, BcGame, BcPlayer]), 
					Num + 1 
				end, 1, SortedBcPlayers),
	{reply, spawn_player_bases(SortedBcPlayers, BaseBcCollisions, State), State}.

terminate(Reason, State) ->
	io:format("BcGameFsm terminates with ~p~n", [Reason]).

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

create_base_entity(BaseBcCollision, BcPlayer, BcEntities) ->
	BaseUuid = bc_collision:uuid(BaseBcCollision),
	BaseBcVertices = bc_collision:vertices(BaseBcCollision),
	{ok, BaseBcEntityConfig} = bc_entities:entity_config(base, BcEntities),
	BaseUuidStr = uuid:uuid_to_string(BaseUuid),
	PlayerId = bc_player:id(BcPlayer),
	Team = bc_player:team(BcPlayer),
	Health = bc_entity_config:health(BaseBcEntityConfig),
	bc_entity:init(BaseUuidStr, PlayerId, Team, base, 
				   Health, Health, undefined, BaseBcVertices).

spawn_base_ai(BaseBcEntity, #state{input_sup = BcInputSup,
								   map = BcMap,
								   entities = BcEntities} = State) ->
	PlayerId = bc_entity:player_id(BaseBcEntity),
	case lists:filter(fun({Id, _, _, _}) -> 
						Id =:= PlayerId 
					  end, supervisor:which_children(BcInputSup)) of
		[{_, BcPlayerSup, _, _}] ->
			{ok, BaseBcAiFsm} = supervisor:start_child(BcPlayerSup, #{
				id => PlayerId,
				start => {bc_ai_fsm, start_link, [BaseBcEntity, BcEntities, BcMap]},
				restart => permanent,
				type => worker,
				modules => [bc_ai_fsm]
			}),
			UpdatedBaseBcEntity = bc_entity:set_ai_fsm(BaseBcAiFsm, BaseBcEntity),
			{ok, UpdatedBaseBcEntity};
		[] ->
			{error, "Can't find player supervisor."}
	end.

insert_base_entity(BaseBcEntity, BcEntities) ->
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
spawn_player_bases([BcPlayer|BcPlayers], [BaseBcCollision|BaseBcCollisions], 
				   #state{input_sup = BcInputSup,
						  map = BcMap, 
						  entities = BcEntities} = State) ->
	case bc_map:insert_collision(BcMap, BaseBcCollision) of
		true ->
			BaseBcEntity = create_base_entity(BaseBcCollision, BcPlayer, BcEntities),
			case spawn_base_ai(BaseBcEntity, State) of
				{ok, UpdatedBaseBcEntity} ->
					insert_base_entity(UpdatedBaseBcEntity, BcEntities),
					spawn_player_bases(BcPlayers, BaseBcCollisions, State);
				{error, Reason} = Error ->
					Error
			end;
		false ->
			{error, "Could not spawn base for player " ++ bc_player:id(BcPlayer)}
	end.
