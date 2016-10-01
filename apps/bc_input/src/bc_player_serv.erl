
-module(bc_player_serv).
-behavior(gen_server).

%% api functions
-export([start_link/6,
		 spawn_entities/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% state rec
-record(state, {entity_sup,
				game,
				player,
				base_num,
				spawn_matrix,
				gold,
				map,
				entities}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcEntitySup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities) ->
	gen_server:start_link(?MODULE, [BcEntitySup, 
									BcGame, 
									BcPlayer, 
									BcGoldFsm, 
									BcMap, 
									BcEntities], []).

spawn_entities(BcPlayerServ, EntityType) ->
	gen_server:cast(BcPlayerServ, {spawn_entities, EntityType}).

%%====================================================================
%% Gen_server functions
%%====================================================================

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
init([BcEntitySup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities]) ->
	{ok, #state{entity_sup = BcEntitySup,
				game = BcGame, 
				player = BcPlayer,
				base_num = undefined,
				spawn_matrix = undefined,
				gold = BcGoldFsm,
				map = BcMap,
				entities = BcEntities}}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
handle_call(Request, From, State) ->
    {reply, ok, State}.

-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_cast({spawn_entities, EntityTypeStr}, #state{entities = BcEntities} = State) ->
	EntityType = bc_entity_util:iolist_to_entity_type(EntityTypeStr),
	case bc_entities:entity_config(EntityType, BcEntities) of
		{ok, BcEntityConfig} ->
			{ok, BaseNum, State1} = player_num(State),
			{ok, BcMatrix, State2} = spawn_matrix(State1),
			Cost = bc_entity_config:cost(BcEntityConfig),
			case bc_gold_fsm:subtract(State2#state.gold, Cost) of
				{ok, _} ->					
					spawn_entity_batch(BcEntityConfig, State2),
					{noreply, State2};
				{error, _} ->
					{noreply, State2}
			end;
		error ->
			{noreply, State}
	end.

-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_info(Info, State) ->
    {noreply, State}.

-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
terminate(Reason, State) ->
	io:format("BcPlayerServer terminates with ~p~n", [Reason]),
    ok.

-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
code_change(OldVsn, State, Extra) ->
    {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

base_num(BaseBcVertices, BcMap) ->
	BaseBcVertex = lists:nth(1, BaseBcVertices),
	case lists:any(fun(Base1BcVertex) -> 
					  bc_vertex:row(Base1BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso
						  bc_vertex:col(Base1BcVertex) =:= bc_vertex:col(BaseBcVertex)
				   end, bc_map:base1_vertices(BcMap)) of
		true -> 1;
		false -> 
			case lists:any(fun(Base2BcVertex) ->
						       bc_vertex:row(Base2BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso
								   bc_vertex:col(Base2BcVertex) =:= bc_vertex:col(BaseBcVertex) 
				   		   end, bc_map:base2_vertices(BcMap)) of
				true -> 2;
				false ->
					case lists:any(fun(Base3BcVertex) -> 
								       bc_vertex:row(Base3BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso 
										   bc_vertex:col(Base3BcVertex) =:= bc_vertex:col(BaseBcVertex) 
								   end, bc_map:base3_vertices(BcMap)) of
						true -> 3;
						false ->
							case lists:any(fun(Base4BcVertex) -> 
								       			bc_vertex:row(Base4BcVertex) =:= bc_vertex:row(BaseBcVertex) andalso 
													bc_vertex:col(Base4BcVertex) =:= bc_vertex:col(BaseBcVertex) 
										   end, bc_map:base4_vertices(BcMap)) of
								true -> 4;
								false -> undefined
							end
					end
			end
	end.

player_num(#state{player = BcPlayer,
				  base_num = BaseNum,
				  map = BcMap, 
				  entities = BcEntities} = State) ->
	case BaseNum of
		undefined ->
			PlayerId = bc_player:id(BcPlayer),
			BaseBcEntities = bc_entities:query_type(base, BcEntities),
			case lists:filter(fun(BaseBcEntity) -> 
								  PlayerId =:= bc_entity:player_id(BaseBcEntity) 
							  end, BaseBcEntities) of
				[] ->
					{ok, undefined, State};
				[PlayerBaseBcEntity] ->
					Uuid = bc_entity:uuid(PlayerBaseBcEntity),
					QueryResults = bc_map:query_ids(BcMap, Uuid),
					BcVertices = lists:map(fun(#{vertex := BcVertex}) -> 
											  BcVertex 
										   end, QueryResults),
					Num = base_num(BcVertices, BcMap),
					{ok, Num, State#state{base_num = Num}}
			end;
		Num when is_integer(Num) ->
			{ok, Num, State}
	end.

spawn_entity_batch(BcEntityConfig, #state{spawn_matrix = SpawnBcMatrix} = State) ->
	case spawn_vertices(0, State) of
		{ok, SpawnBcVertices} ->
			EntitySize = bc_entity_config:size(BcEntityConfig),
			SpawnCountFloat = length(SpawnBcVertices) / EntitySize,
			SpawnCount = erlang:trunc(SpawnCountFloat),
			case bc_matrix:dimensions(SpawnBcMatrix) of
				{RowCount, ColCount} when RowCount >= ColCount ->
					spawn_entities({0, RowCount}, SpawnCount, BcEntityConfig, State);
				{RowCount, ColCount} when RowCount < ColCount ->
					spawn_entities({0, ColCount}, SpawnCount, BcEntityConfig, State);
				_ ->
					{error, "Cannot spawn entities."}
			end;
		error ->
			{error, "Cannot spawn entities."}
	end.
	
spawn_entities({Offset, MaxOffset}, _, _, _) when Offset > MaxOffset ->
	ok;
spawn_entities(_, SpawnCount, _, _) when SpawnCount =< 0 ->
	ok;
spawn_entities({Offset, MaxOffset}, SpawnCount, BcEntityConfig, 
			   #state{entities = BcEntities,
					  map = BcMap,
					  base_num = BaseNum,
					  spawn_matrix = BcMatrix} = State) ->
	case spawn_vertices(Offset, State) of
		{ok, SpawnBcVertices} ->
			SpawnedCount = do_spawn_entities(SpawnCount, 0, SpawnBcVertices, 
											 BcEntityConfig, State),
			spawn_entities({Offset + 1, MaxOffset}, SpawnCount - SpawnedCount, 
						   BcEntityConfig, State);
		error ->
			{error, "Cannot spawn entities"}
	end.
	
do_spawn_entities(0, Acc, _, _, _) ->
	Acc;
do_spawn_entities(_, Acc, [], _, _) ->
	Acc;
do_spawn_entities(BatchCount, Acc, [SpawnBcVertex|SpawnBcVertices], 
			   BcEntityConfig,  #state{entity_sup = BcEntitySup,
									   entities = BcEntities,
									   player = BcPlayer,
					  				   map = BcMap,
					  				   base_num = BaseNum} = State) ->
	Uuid = uuid:get_v4(),
	BcCollision = bc_collision:init(Uuid, SpawnBcVertex),
	Orientation = entity_orientation(BaseNum),
	case bc_entity_util:spawn_entity(BcCollision, BcPlayer, BcEntitySup, BcEntityConfig, 
									 Orientation, BcMap, BcEntities) of
		{ok, BcEntity} ->
			do_spawn_entities(BatchCount - 1, Acc + 1, SpawnBcVertices, 
							  BcEntityConfig, State);
		_ ->
			do_spawn_entities(BatchCount, Acc, SpawnBcVertices, 
							  BcEntityConfig, State)
	end.

spawn_vertices(Offset, #state{base_num = BaseNum,
					  		  spawn_matrix = SpawnBcMatrix} = State) ->
	case BaseNum of
		1 -> 
			MinRow = bc_matrix:min_row(SpawnBcMatrix),
			bc_matrix:row(MinRow + Offset, SpawnBcMatrix);
		2 ->
			MaxCol = bc_matrix:max_col(SpawnBcMatrix),
			bc_matrix:col(MaxCol - Offset, SpawnBcMatrix);
		3 ->
			MaxRow = bc_matrix:max_row(SpawnBcMatrix),
			bc_matrix:row(MaxRow - Offset, SpawnBcMatrix);
		4 ->
			MinCol = bc_matrix:min_col(SpawnBcMatrix),
			bc_matrix:col(MinCol + Offset, SpawnBcMatrix)
	end.

entity_orientation(BaseNum) ->
	case BaseNum of
		1 -> down;
		2 -> left;
		3 -> up;
		4 -> right;
		_ -> up
	end.

spawn_matrix(#state{base_num = BaseNum,
					spawn_matrix = SpawnBcMatrix,
					map = BcMap} = State) ->
	case SpawnBcMatrix of
		undefined ->
			BcVertices = bc_map:base_vertices(BcMap, BaseNum),
			BcMatrix = bc_matrix:init(BcVertices),
			{ok, BcMatrix, State#state{spawn_matrix = BcMatrix}};
		BcMatrix ->
			{ok, BcMatrix, State}
	end.
