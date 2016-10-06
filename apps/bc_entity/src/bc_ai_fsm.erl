
-module(bc_ai_fsm).
-behaviour(gen_fsm).

%% distance to sense in vertices
-define(SENSE_DIST, 4).

-export([init/1, 
		 no_action/2,
		 standing/2,
		 moving/2, 
		 attacking/2,
		 
		 %% TODO implement actions/3
		  
		 handle_event/3, 
		 handle_sync_event/4, 
		 handle_info/3, 
		 terminate/3, 
		 code_change/4]).

-export([start_link/3,
		 entity_died/2,
		 damage_entity/2]).

%% internal state
-record(state, {entity,
				entity_config,
				entities,
				map,
				entity_event_handler,
				path,
				targets,
				timer}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start_link(BcEntity :: bc_entity:entity(), 
				 BcEntities :: bc_entities:entities(), 
				 BcMap :: bc_map:map_graph()) -> gen:start_ret().
start_link(BcEntity, BcEntities, BcMap) ->
	gen_fsm:start_link(?MODULE, [BcEntity, BcEntities, BcMap], []).

-spec entity_died(BcAiFsm :: pid(), 
				  EntityDiedEvent :: 
					  {entity_died, bc_entity:entity()}) -> ok.
entity_died(BcAiFsm, EntityDiedEvent) ->
	gen_fsm:send_all_state_event(BcAiFsm, EntityDiedEvent).

-spec damage_entity(BcAiFsm :: pid(), 
					Damage :: integer()) -> ok.
damage_entity(BcAiFsm, Damage) ->
	gen_fsm:send_all_state_event(BcAiFsm, {entity_damaged, Damage}).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([BcEntity, BcEntities, BcMap]) ->
	UpdatedBcEntity = bc_entity:set_ai_fsm(self(), BcEntity),
	EntityType = bc_entity:entity_type(UpdatedBcEntity),
	{ok, BcEntityConfig} = bc_entities:entity_config(EntityType, BcEntities),
	StateName = 
		case bc_entity_config:entity_class(BcEntityConfig) of 
			structure -> no_action; 
			_ -> standing 
		end,
	TimerRef = gen_fsm:send_event_after(5, action_complete),
	%% properly seed with <<A:32, B:32, C:32>> = crypto:rand_bytes(12),
	_ = rand:seed(exs1024, {erlang:unique_integer([positive]), 
							erlang:unique_integer([positive]), 
							erlang:unique_integer([positive])}),
	{ok, StateName, #state{entity = UpdatedBcEntity,
						   entity_config = BcEntityConfig, 
						   entity_event_handler = undefined,
					   	   entities = BcEntities, 
					   	   map = BcMap,
						   path = undefined,
						   targets = [],
						   timer = TimerRef}}.

no_action(action_complete, State) ->
	{next_state, no_action, State}.

standing(action_complete, State) ->
	sense(State).

moving(action_complete, State) ->
	sense(State).

attacking(action_complete, State) ->
	sense(State).

%% ====================================================================
%% All State functions
%% ====================================================================

handle_event({entity_died, EnemyBcEntity}, StateName, #state{timer = TimerRef} = State) ->
	case StateName of
		attacking ->
			gen_fsm:cancel_timer(TimerRef),
			sense(State#state{entity_event_handler = undefined,
							  path = undefined,
							  targets = [],
							  timer = undefined});
		_ ->
    		{next_state, StateName, 
			 State#state{entity_event_handler = undefined,
						 path = undefined,
						 targets = []}}
	end;
handle_event({entity_damaged, Damage}, StateName, #state{entity = BcEntity,
														 entities = BcEntities,
														 map = BcMap} = State) ->
	case bc_entity:health(BcEntity) - Damage of
		UpdatedHealth when UpdatedHealth > 0 ->
			DamagedEntity = bc_entity:set_health(BcEntity, UpdatedHealth),
			bc_entities:insert(DamagedEntity, BcEntities),
			EntitiesEventPid = bc_entities:event(BcEntities),
			gen_event:notify(EntitiesEventPid, {entity_damaged, DamagedEntity}),
			%% TODO if standing cancel timer and sense again
			{next_state, StateName, State#state{entity = DamagedEntity}};
		UpdatedHealth when UpdatedHealth =< 0 ->
			DeadEntity = bc_entity:set_health(BcEntity, UpdatedHealth),
			BcCollision = bc_entity:to_collision(BcEntity),
			bc_map:delete_collision(BcMap, BcCollision),
			Uuid = bc_entity:uuid(BcEntity),
			bc_entities:delete(Uuid, BcEntities),
			EntitiesEventPid = bc_entities:event(BcEntities),
			gen_event:notify(EntitiesEventPid, {entity_died, DeadEntity}),
			{stop, normal, State#state{entity = DeadEntity}}
	end;
handle_event(Event, StateName, StateData) ->
  {next_state, StateName, StateData}.

handle_sync_event(Event, From, StateName, StateData) ->
    Reply = ok,
    {reply, Reply, StateName, StateData}.

%% ====================================================================
%% Gen_* functions
%% ====================================================================

handle_info(Info, StateName, StateData) ->
    {next_state, StateName, StateData}.

terminate(Reason, StateName, StatData) ->
	lager:info("BcAiFsm crashed with reason: ~p", [Reason]),
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

sense(#state{entity = BcEntity,
			 entity_config = BcEntityConfig,
			 map = BcMap} = State) ->
	NearbyBcEntities = nearby_entities(State),
	case lists:filter(fun(NearbyBcEntity) -> 
						bc_entity:team(NearbyBcEntity) /= 
							bc_entity:team(BcEntity)
					  end, NearbyBcEntities) of
		EnemyBcEntites when length(EnemyBcEntites) > 0 ->
			plan_enemy_actions(EnemyBcEntites, NearbyBcEntities, State);
		_ ->
			move_enemy_base(State)
	end.

plan_enemy_actions(EnemyBcEntities, NearbyBcEntities, 
				   #state{entity = BcEntity} = State) ->
	InRangeEnemyBcEntityDict = inrange_entity_dict(EnemyBcEntities, State),
	case in_range_enemies(InRangeEnemyBcEntityDict, BcEntity) of
		InRangeEnemyBcEntities when length(InRangeEnemyBcEntities) > 0 ->
			attack_entities(InRangeEnemyBcEntities, State);
		_ ->
			move_in_range(InRangeEnemyBcEntityDict, NearbyBcEntities, State)
	end.

inrange_entity_dict(EnemyBcEntities, #state{entity = BcEntity,
											entity_config = BcEntityConfig,
											map = BcMap} = State) ->
	Range = bc_entity_config:range(BcEntityConfig),
	lists:foldl(
	  fun(EnemyBcEntity, AccEnemyBcVertexDict) ->
		  EnemyBcVertices = bc_entity:vertices(EnemyBcEntity),
		  InRangeBcVertices = 
			  bc_map:reaching_neighbors(BcMap, EnemyBcVertices, Range),
		  lists:foldl(
			fun(InRangeBcVertex, AccAccEnemyBcVertexDict) -> 
				EnemyBcEntitySet =					
					case dict:find(InRangeBcVertex, AccAccEnemyBcVertexDict) of
						{ok, BcEntitySet} ->
							BcEntitySet;
						error ->
							sets:new()
					end,
				UpdatedSet = sets:add_element(EnemyBcEntity, EnemyBcEntitySet),
				dict:store(InRangeBcVertex, UpdatedSet, AccAccEnemyBcVertexDict)
			end, AccEnemyBcVertexDict, InRangeBcVertices)
	  end, dict:new(), EnemyBcEntities).

in_range_enemies(InRangeEnemyBcEntityDict, BcEntity) ->
	BcVertices = bc_entity:vertices(BcEntity),
	lists:foldl(
	  fun(BcVertex, AccEnemyBcEntities) -> 
		  case dict:find(BcVertex, InRangeEnemyBcEntityDict) of 
			  {ok, EnemyBcEntitySet} ->
				  EnemyBcEntities = sets:to_list(EnemyBcEntitySet),
				  lists:append(AccEnemyBcEntities, EnemyBcEntities);
			  error ->
				  AccEnemyBcEntities
		  end
	  end, [], BcVertices).

attack_entities(InRangeEnemyBcEntities, State) ->
	case lists:filter(fun(EnemyBcEntity) ->
						bc_entity:entity_type(EnemyBcEntity) == base
					  end, InRangeEnemyBcEntities) of
		InRangeEnemyBaseBcEntity when length(InRangeEnemyBaseBcEntity) > 0 ->
			EnemyBaseBcEntity = lists:nth(1, InRangeEnemyBaseBcEntity),
			attack_entity(EnemyBaseBcEntity, State);
		_ ->
			InRangeEnemyBcEntity = lists:nth(1, InRangeEnemyBcEntities),
			EnemyBcEntity = 
				lists:foldl(
				    fun(BcEntity, AccBcEntity) ->
						case bc_entity:health(BcEntity) < 
								 bc_entity:health(AccBcEntity) of
							true -> BcEntity;
							false -> AccBcEntity
						end 
					end, InRangeEnemyBcEntity, InRangeEnemyBcEntities),
			attack_entity(InRangeEnemyBcEntity, State)
	end.

attack_entity(EnemyBcEntity, #state{entity = BcEntity,
									entity_config = BcEntityConfig,
									entities = BcEntities, 
									entity_event_handler = EventHandler} = State) ->
	Orientation = determine_orientation(BcEntity, EnemyBcEntity),
	ReorientedBcEntity = reorient_entity(Orientation, BcEntity, BcEntities),
	EnemyUuid = bc_entity:uuid(EnemyBcEntity),
	UpdatedEventHandler =
		case EventHandler of
			undefined ->
				EntitiesEventPid = bc_entities:event(BcEntities),
				gen_event:notify(EntitiesEventPid, {entity_attacking, ReorientedBcEntity}),
				EntityUuid = bc_entity:uuid(ReorientedBcEntity),
				EntitiesEventPid = bc_entities:event(BcEntities),
				Handler = {bc_died_event, EntityUuid},
				Result = gen_event:add_sup_handler(EntitiesEventPid, 
												   Handler, 
												   [EnemyUuid, self()]),
				Handler;
			_ ->
				EventHandler
		end,
	EnemyBcAiFsm = bc_entity:ai_fsm(EnemyBcEntity),
	Damage = calculate_damage(BcEntityConfig, EnemyBcEntity, BcEntities),
	bc_ai_fsm:damage_entity(EnemyBcAiFsm, Damage),
	AttackSpeed = bc_entity_config:attack_speed(BcEntityConfig),
	TimerRef = send_action_complete(AttackSpeed),
	{next_state, attacking, State#state{entity = ReorientedBcEntity,
										entity_event_handler = UpdatedEventHandler,
										targets = [EnemyBcEntity],
										timer = TimerRef}}.

calculate_damage(BcEntityConfig, EnemyBcEntity, BcEntities) ->
	EnemyEntityType = bc_entity:entity_type(EnemyBcEntity),
	{ok, EnemyBcEntityConfig} = bc_entities:entity_config(EnemyEntityType, BcEntities),
	EntityClass = bc_entity_config:entity_class(BcEntityConfig),
	EnemyEntityClass = bc_entity_config:entity_class(EnemyBcEntityConfig),
	Modifier = 
		case {EntityClass, EnemyEntityClass} of
			{infantry, light_armor} -> 0.8;
			{infantry, armor} -> 0.65;
			{anti_infantry, infantry} -> 1.35;
			{anti_infantry, light_armor} -> 0.75;
			{anti_infantry, armor} -> 0.7;
			{armor, structure} -> 1.3;
			{light_armor, structure} -> 1.2;
			_ -> 1.0
		end,
	{MinDamage, MaxDamage} = bc_entity_config:damage(BcEntityConfig),
	{ModMinDamage, ModMaxDamage} = {erlang:trunc(MinDamage * Modifier), 
									erlang:trunc(MaxDamage * Modifier)},
	DamageDiff = ModMaxDamage - ModMinDamage,
	RandDamage = rand:uniform(DamageDiff),
	ModMinDamage + RandDamage.

move_in_range(InRangeEnemyBcEntityDict, NearbyBcEntities, 
			  #state{entity = BcEntity, map = BcMap} = State) ->
	InRangeBcVertices = dict:fetch_keys(InRangeEnemyBcEntityDict),
	case choose_path(InRangeBcVertices, NearbyBcEntities, State) of
		PathBcVertices when is_list(PathBcVertices) 
		  andalso length(PathBcVertices) > 0 ->
			DestinationBcVertex = lists:last(PathBcVertices),
			Targets =
				case dict:find(DestinationBcVertex, InRangeEnemyBcEntityDict) of
					{ok, EnemyBcEntitiesSet} ->
						sets:to_list(EnemyBcEntitiesSet);
					error ->
						[]
				end,
			move_on_path(State#state{path = PathBcVertices,
									 targets = Targets});
		undefined ->
			%% TODO move closer to enemy if possible
			stand(State#state{path = undefined,
							  targets = []})
	end.

move_on_path(#state{entity = BcEntity,
					entities = BcEntities,
					path = PathBcVertices,
					targets = Targets} = State) ->
	case lists:any(
		   fun(EnemyBcEntity) -> 
			   Uuid = bc_entity:uuid(EnemyBcEntity),
			   bc_entities:exists(Uuid, BcEntities)
		   end, Targets) of
		true ->			
			MoveBcVertex = lists:nth(1, PathBcVertices),
			Direction = bc_entity_util:move_direction(BcEntity, MoveBcVertex),
			move(Direction, State);
		false ->
			sense(State#state{path = undefined,
							  targets = []})
	end.
	
choose_path(InRangeBcVertices, NearbyBcEntities, 
			#state{entity = BcEntity, map = BcMap} = State) ->
	SortedDistBcVertices = sorted_dist_vertices(InRangeBcVertices, State),
	OccupiedBcVertices = 
		lists:flatmap(
			fun(NearbyBcEntity) -> 
				bc_entity:vertices(NearbyBcEntity) 
			end, NearbyBcEntities),
	UnoccupiedSortedDistBcVertices =
		lists:filter(
		   fun({_Dist, BcVertex}) ->
			   not lists:member(BcVertex, OccupiedBcVertices)
		   end, SortedDistBcVertices),
	lists:foldl(
	  fun({_Dist, InRangeBcVertex}, AccPath) -> 
		  case AccPath of 
			  Path when is_list(Path) andalso length(Path) > 0 -> 
				  Path;
			  undefined -> 
				  PathBcVertices = compute_path(BcEntity, InRangeBcVertex, State),
				  %% TODO there is deffinitly a faster way to do this
				  case lists:any(
						 fun(PathBcVertex) ->
							 lists:member(PathBcVertex, OccupiedBcVertices)
						 end, PathBcVertices) of
					  true -> undefined;
					  false -> PathBcVertices
				  end
		  end 
	  end, undefined, UnoccupiedSortedDistBcVertices).

sorted_dist_vertices(InRangeBcVertices, #state{entity = BcEntity} = State) ->
	DistBcVertices = 
		lists:map(
			fun(BcVertex) -> 
				Distance =
					bc_entity_util:vertex_distance(BcEntity, BcVertex),
				{Distance, BcVertex} 
			end, InRangeBcVertices),
	lists:sort(fun({Dist1, _}, 
				   {Dist2, _}) ->
				 Dist1 =< Dist2
			   end, DistBcVertices).

compute_path(BcEntity, BcVertex, #state{map = BcMap} = State) ->
	ClosestEntityBcVertex = 
		bc_entity_util:closest_entity_vertex(BcEntity, BcVertex),
	case bc_map:compute_path(BcMap, ClosestEntityBcVertex, BcVertex) of
		Path when is_list(Path) andalso length(Path) > 1 ->
			lists:sublist(Path, 2, length(Path) + 1);
		_EmptyPath ->
			[]
	end.

determine_orientation(BcEntity, EnemyBcEntity) ->
	BcVertices = bc_entity:vertices(BcEntity),
	BcMatrix = bc_matrix:init(BcVertices),
	EnemyBcVertices = bc_entity:vertices(EnemyBcEntity),
	EnemyBcMatrix = bc_matrix:init(EnemyBcVertices),
	RowSet = bc_matrix:rows(BcMatrix),
	EnemyRowSet = bc_matrix:rows(EnemyBcMatrix),
	case sets:is_disjoint(RowSet, EnemyRowSet) of
		true ->
			MaxRow = bc_matrix:max_row(BcMatrix),
			case bc_matrix:max_row(EnemyBcMatrix) of
				EnemyMaxRow when EnemyMaxRow < MaxRow ->
					up;
				EnemyMaxRow when EnemyMaxRow >= MaxRow ->
					down
			end;
		false ->
			MaxCol = bc_matrix:max_col(BcMatrix),
			case bc_matrix:max_col(EnemyBcMatrix) of
				EnemyMaxCol when EnemyMaxCol < MaxCol ->
					left;
				EnemyMaxCol when EnemyMaxCol >= MaxCol ->
					right
			end
	end.

nearby_entities(#state{entity = BcEntity, 
					   entities = BcEntities, 
					   map = BcMap} = State) ->
	EntityBcVertices = bc_entity:vertices(BcEntity),
	NeighborBcVertices = 
		bc_map:reaching_neighbors(BcMap, EntityBcVertices, ?SENSE_DIST),
	CollisionQueryRes = bc_map:query_collisions(BcMap, NeighborBcVertices),
	UuidSet = 
		lists:foldl(fun(QueryMap, AccSet) ->
						CollUuid = maps:get(uuid, QueryMap),
						sets:add_element(CollUuid, AccSet) 
					end, sets:new(), CollisionQueryRes),
	UuidList = sets:to_list(UuidSet),
	EntityQueryRes = bc_entities:query(UuidList, BcEntities),
	lists:map(fun(QueryBcEntity) ->  
			 	QueryBcVertices = lists:filtermap(
					fun(QueryResult) ->
						QueryUuid = maps:get(uuid, QueryResult),
						QueryBcVertex = maps:get(vertex, QueryResult),
						case QueryUuid =:= bc_entity:uuid(QueryBcEntity) of
							true ->
								{true, QueryBcVertex};
							false ->
								false
						end
					end, CollisionQueryRes),
				bc_entity:set_vertices(QueryBcVertices, QueryBcEntity)
			end, EntityQueryRes).

move_enemy_base(#state{entity_config = BcEntityConfig,
					   entities = BcEntities,
					   map = BcMap,
					   targets = Targets} = State) ->
	case lists:any(
		   fun(TargetBcEntity) -> 
			   bc_entity:entity_type(TargetBcEntity) == base 
		   end, Targets) of
		true ->
			move_on_path(State);
		false ->
			{BaseBcEntity, UpdatedState} = enemy_base_entity(State),
			BaseUuid = bc_entity:uuid(BaseBcEntity),
			BaseQueryRes = bc_map:query_ids(BcMap, BaseUuid),
			BaseBcVertices = 
				lists:map(
				  fun(QueryRes) ->
					maps:get(vertex, QueryRes)
				  end, BaseQueryRes),
			UpdatedBaseBcEntity = 
				bc_entity:set_vertices(BaseBcVertices, BaseBcEntity),
			InRangeEnemyBcEntityDict = inrange_entity_dict([UpdatedBaseBcEntity], UpdatedState),
			NearbyEntities = nearby_entities(UpdatedState),
			move_in_range(InRangeEnemyBcEntityDict, NearbyEntities, UpdatedState)
	end.

enemy_base_entity(#state{entities = BcEntities,
						 targets = Targets} = State) ->
	case lists:filter(
		   fun(TargetBcEntity) ->
			   Uuid = bc_entity:uuid(TargetBcEntity),
			   EntityType = bc_entity:entity_type(TargetBcEntity),
			   EntityType == base andalso
				   bc_entities:exists(Uuid, BcEntities)
		   end, Targets) of
		BaseBcEntities when length(BaseBcEntities) > 0 ->
			BaseBcEntity = lists:nth(1, BaseBcEntities),
			{BaseBcEntity, State};
		_ ->
			BaseBcEntity = find_enemy_base(State),
			{BaseBcEntity, State#state{targets = [BaseBcEntity]}}
	end.

find_enemy_base(#state{entity = BcEntity, 
					   entities = BcEntities,
					   map = BcMap} = State) ->
	PlayerNum = bc_entity:player_num(BcEntity),
	EnemyNums = 				
		case PlayerNum of
			1 -> [3, 4];
			2 -> [4, 1];
			3 -> [1, 2];
			4 -> [2, 1];
			_ -> []
		end,
	BaseBcEntities = bc_entities:query_type(base, BcEntities),
	case lists:map(
		   fun(BaseBcEntity) -> 
			  BasePlayerNum = bc_entity:player_num(BaseBcEntity),
			  {BasePlayerNum, BaseBcEntity}
		   end, BaseBcEntities) of
		PlayerNumBaseBcEntities when length(PlayerNumBaseBcEntities) > 0 ->
			EnemyBaseBcEntities =
				lists:map(
				  fun(EnemyNum) ->
					  case lists:keyfind(EnemyNum, 1, PlayerNumBaseBcEntities) of
						  {BasePlayerNum, BaseBcEntity} ->
							  BaseBcEntity;
						  false -> 
							  undefined
					  end 
				  end, EnemyNums),
			lists:foldl(
			  fun(BaseBcEntity, AccBaseBcEntity) -> 
				  case AccBaseBcEntity of 
					  undefined -> 
						  BaseBcEntity;
					  AccBaseBcEntity ->
						  AccBaseBcEntity
				  end 
			  end, undefined, EnemyBaseBcEntities);
		[] ->
			undefined
	end.

move(Direction, #state{entity = BcEntity, 
					   entity_config = BcEntityConfig,		 
					   entities = BcEntities,
					   map = BcMap,
					   path = Path} = State) ->
	OriginalBcCollision = bc_entity:to_collision(BcEntity),
	UpdatedBcCollision = bc_collision:move(Direction, OriginalBcCollision),
	UpdatedBcVertices = bc_collision:vertices(UpdatedBcCollision),
	case bc_map:are_vertices(BcMap, UpdatedBcVertices) of
		true ->
			case bc_map:update_collision(BcMap, OriginalBcCollision, UpdatedBcCollision) of
				ok ->
					MovedBcEntity = bc_entity:set_vertices(UpdatedBcVertices, BcEntity),
					ReorientedBcEntity = reorient_entity(Direction, MovedBcEntity, BcEntities),
					case update_path(UpdatedBcVertices, Path) of
						undefined ->							
							on_moved(State#state{entity = ReorientedBcEntity,
												 path = undefined,
												 targets = []});
						UpdatedPath ->
							on_moved(State#state{entity = ReorientedBcEntity,
												 path = UpdatedPath})
					end;
				{error, _} ->
					stand(State)
			end;
		false ->
			stand(State)
	end.

stand(State) ->
	{next_state, standing, State#state{timer = 
										gen_fsm:send_event_after(100, action_complete)}}.

update_path(_, undefined) ->
	undefined;
update_path(_, Path) when length(Path) == 0 ->
	undefined;
update_path(UpdatedBcVertices, Path) ->
	MoveBcVertex = lists:nth(1, Path),
	case lists:member(MoveBcVertex, UpdatedBcVertices) of
		true ->
			case lists:sublist(Path, 2, length(Path)) of
				UpdatedPath when length(UpdatedPath) > 0 ->
					UpdatedPath;
				_ ->
					undefined
			end;
		false ->
			undefined
	end.	

on_moved(#state{entity = BcEntity,
				entity_config = BcEntityConfig,
				entities = BcEntities} = State) ->
	EntitiesEventPid = bc_entities:event(BcEntities),
	gen_event:notify(EntitiesEventPid, {entity_moved, BcEntity}),
	MoveSpeed = bc_entity_config:move_speed(BcEntityConfig),
	TimerRef = send_action_complete(MoveSpeed),
	{next_state, moving, State#state{timer = TimerRef}}.

reorient_entity(Orientation, BcEntity, BcEntities) ->
	case bc_entity:orientation(BcEntity) =:= Orientation of
		true ->
			BcEntity;
		false ->
			UpdatedBcEntity = bc_entity:set_orientation(BcEntity, Orientation),
			bc_entities:insert(UpdatedBcEntity, BcEntities),
			UpdatedBcEntity
	end.

send_action_complete(Speed) ->
	DelayFloat = 1000 - (1000 * Speed),
	DelayInt = erlang:trunc(DelayFloat),
	gen_fsm:send_event_after(DelayInt, action_complete).

