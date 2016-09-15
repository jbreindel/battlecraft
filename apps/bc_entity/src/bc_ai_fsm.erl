
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
				entity_event_handler,
				player_num,
				map,
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
	StateName = case bc_entity_config:entity_class(BcEntityConfig) of 
					structure -> no_action; 
					_ -> standing 
				end,
	PlayerNum =  case bc_entity:orientation(BcEntity) of
					 down -> 1;
					 left -> 2;
					 up -> 3;
					 right -> 4;
					 _ -> 0
				 end,
	TimerRef = gen_fsm:send_event_after(5, action_complete),
	{ok, StateName, #state{entity = UpdatedBcEntity,
						   entity_config = BcEntityConfig, 
						   entity_event_handler = undefined,
					   	   entities = BcEntities, 
						   player_num = PlayerNum,
					   	   map = BcMap,
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

handle_event({entity_died, EnemyBcEntity}, StateName, State) ->
	case StateName of
		attacking ->
			sense(State#state{entity_event_handler = undefined});
		_ ->
    		{next_state, StateName, State}
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
	io:format("BcAiFsm crashed with reason: ~p~n", [Reason]),
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

sense(#state{entity = BcEntity,
			 entity_config = BcEntityConfig,
			 map = BcMap} = State) ->
	DistEntities = dist_entities(State),
	case lists:filter(fun({_, NearbyBcEntity}) -> 
						bc_entity:team(NearbyBcEntity) /= 
							bc_entity:team(BcEntity)
					  end, DistEntities) of
		EnemyDistEntites when length(EnemyDistEntites) > 0 ->
			plan_enemy_actions(DistEntities, EnemyDistEntites, State);
		_ ->
			move_forward(State)
	end.

plan_enemy_actions(DistEntities, EnemyDistEntities, 
				   #state{entity = BcEntity, 
						  entity_config = BcEntityConfig, 
						  map = BcMap} = State) ->
	Range = bc_entity_config:range(BcEntityConfig),
	case lists:filtermap(fun({Dist, EnemyBcEntity}) -> 
							case Dist =< Range of 
								true -> {true, EnemyBcEntity};
								false -> false 
							end 
						end, EnemyDistEntities) of
		InRangeEnemyBcEntities when length(InRangeEnemyBcEntities) > 0 ->
			attack_entities(InRangeEnemyBcEntities, State);
		_ ->
			{_, EnemyBcEntity} = closest_dist_entity(EnemyDistEntities),
			move_in_range(EnemyBcEntity, Range, DistEntities, State)
	end.

closest_dist_entity(EnemyDistEntities) ->
	FirstDistEntity = lists:nth(1, EnemyDistEntities),
	lists:foldl(fun({Dist1, _} = DistEntity,
					{Dist2, _} = AccDistEntity) -> 
					case Dist1 < Dist2 of
						true -> DistEntity;
						false -> AccDistEntity
					end
				end, FirstDistEntity, EnemyDistEntities).

move_in_range(EnemyBcEntity, Range, DistEntities, #state{entity = BcEntity, 
														 map = BcMap} = State) ->
	EnemyBcVertices = bc_entity:vertices(EnemyBcEntity),
	InRangeBcVertices = bc_map:reaching_neighbors(BcMap, EnemyBcVertices, Range),
	DistBcVertices = 
		lists:map(fun(BcVertex) -> 
					Distance =
						bc_entity_util:vertex_distance(BcEntity, BcVertex),
					{Distance, BcVertex} 
				  end, InRangeBcVertices),
	PathBcVertices = choose_path(DistBcVertices, DistEntities, State),
	MoveBcVertex = lists:nth(1, PathBcVertices),
	Direction = bc_entity_util:move_direction(BcEntity, MoveBcVertex),
	move(Direction, State).
	
choose_path(DistBcVertices, DistEntities, #state{entity = BcEntity,
												 map = BcMap} = State) ->
	SortedDistBcVertices = 
		lists:sort(fun({Dist1, _}, 
					   {Dist2, _}) -> 
						Dist1 =< Dist2 
				   end, DistBcVertices),
	{_, ClosestBcVertex} = lists:nth(1, SortedDistBcVertices),
	EntitiesBcVertices = lists:flatten(
		lists:map(fun({_, NearbyBcEntity}) -> 
					bc_entity:vertices(NearbyBcEntity) 
				  end, DistEntities)
	),
	case lists:foldl(fun({_, BcVertex}, AccPath) -> 
						case AccPath of
							Path when is_list(Path) andalso length(Path) > 0 ->
								Path;
							undefined ->  
								PathBcVertices = compute_path(BcEntity, ClosestBcVertex, State),
								%% TODO there is deffinitly a faster way to do this
								case lists:any(fun(PathBcVertex) -> 
												 lists:member(PathBcVertex, EntitiesBcVertices)
											   end, PathBcVertices) of
									true -> undefined;
									false -> PathBcVertices
								end
						end 
					end, undefined, SortedDistBcVertices) of
		Path when is_list(Path) andalso length(Path) > 0 ->
			Path;
		undefined ->
			{_, FirstBcVertex} = lists:nth(1, SortedDistBcVertices),
			compute_path(BcEntity, FirstBcVertex, State)
	end.

compute_path(BcEntity, BcVertex, #state{map = BcMap} = State) ->
	ClosestEntityBcVertex = 
		bc_entity_util:closest_entity_vertex(BcEntity, BcVertex),
	bc_map:compute_path(BcMap, ClosestEntityBcVertex, BcVertex).

attack_entities(InRangeEnemyBcEntities, State) ->
	case lists:filter(fun(EnemyBcEntity) ->
						bc_entity:entity_type(EnemyBcEntity) == base
					  end, InRangeEnemyBcEntities) of
		InRangeEnemyBaseBcEntity when length(InRangeEnemyBaseBcEntity) > 0 ->
			EnemyBaseBcEntity = lists:nth(1, InRangeEnemyBaseBcEntity),
			attack_entity(EnemyBaseBcEntity, State);
		_ ->
			InRangeEnemyBcEntity = lists:nth(1, InRangeEnemyBcEntities),
			attack_entity(InRangeEnemyBcEntity, State)
	end.

attack_entity(InRangeEnemyBcEntity, #state{entity = BcEntity,
										   entity_config = BcEntityConfig,
										   entities = BcEntities,
										   entity_event_handler = EventHandler} = State) ->
	%% TODO change entity orientation if neccesary
	EntitiesEventPid = bc_entities:event(BcEntities),
	gen_event:notify(EntitiesEventPid, {entity_attacking, BcEntity}),
	UpdatedState =
		case EventHandler of
			undefined ->
				EntityUuid = bc_entity:uuid(BcEntity),
				EnemyUuid = bc_entity:uuid(InRangeEnemyBcEntity),
				EntitiesEventPid = bc_entities:event(BcEntities),
				Handler = {bc_entity_died, EntityUuid},
				gen_event:add_sup_handler(EntitiesEventPid, 
										  Handler, 
										  [EnemyUuid, self()]),
				State#state{entity_event_handler = Handler};
			_ ->
				State
		end,
	EnemyBcAiFsm = bc_entity:ai_fsm(InRangeEnemyBcEntity),
	Damage = calculate_damage(BcEntityConfig, InRangeEnemyBcEntity, BcEntities),
	bc_ai_fsm:damage_entity(EnemyBcAiFsm, Damage),
	AttackSpeed = bc_entity_config:attack_speed(BcEntityConfig),
	TimerRef = send_action_complete(AttackSpeed),
	{next_state, attacking, UpdatedState#state{timer = TimerRef}}.

determine_orientation(BcEntity, EnemyBcEntity) ->
	BcVertices = bc_entity:vertices(BcEntity),
	EnemyBcVertices = bc_entity:vertices(EnemyBcEntity),
	%% TODO recalculate orientation
	up.

calculate_damage(BcEntityConfig, EnemyBcEntity, BcEntities) ->
	EnemyEntityType = bc_entity:entity_type(EnemyBcEntity),
	{ok, EnemyBcEntityConfig} = bc_entities:entity_config(EnemyEntityType, BcEntities),
	%% TODO calcuate damage
	10.

dist_entities(#state{entity = BcEntity} = State) ->
	NearbyBcEntities = nearby_entities(State),
	lists:map(fun(NearbyBcEntity) ->
				Distance = bc_entity_util:entity_distance(BcEntity, NearbyBcEntity),
				{Distance, NearbyBcEntity}
			  end, NearbyBcEntities).

nearby_entities(#state{entity = BcEntity, 
					   entities = BcEntities, 
					   map = BcMap} = State) ->
	EntityBcVertices = bc_entity:vertices(BcEntity),
	NeighborBcVertices = bc_map:reaching_neighbors(
						   BcMap, EntityBcVertices, ?SENSE_DIST),
	CollisionQueryRes = bc_map:query_collisions(BcMap, NeighborBcVertices),
	UuidSet = lists:foldl(fun(#{uuid := Uuid}, Set) -> 
						  	  sets:add_element(Uuid, Set) 
						  end, sets:new(), CollisionQueryRes),
	UuidList = sets:to_list(UuidSet),
	EntityQueryRes = bc_entities:query(UuidList, BcEntities),
	lists:map(fun(BcEntity) ->  
			 	BcVertices = lists:filtermap(
					fun(#{uuid := Uuid,
						  vertex := BcVertex}) -> 
						case Uuid =:= bc_entity:uuid(BcEntity) of
							true ->
								{true, BcVertex};
							false ->
								false
						end
					end, CollisionQueryRes),	
				bc_entity:set_vertices(BcVertices, BcEntity)
			end, EntityQueryRes).

move_forward(#state{player_num = PlayerNum} = State) ->
	case PlayerNum of
		1 -> move(down, State);
		2 -> move(left, State);
		3 -> move(up, State);
		4 -> move(right, State)
	end.

move(Direction, #state{entity = BcEntity, 
					   entity_config = BcEntityConfig,		 
					   entities = BcEntities,
					   map = BcMap} = State) ->
	case move_entity(Direction, State) of
		{ok, MovedBcEntity} ->
			ReorientedBcEntity = reorient_entity(Direction, MovedBcEntity, BcEntities),
			EntitiesEventPid = bc_entities:event(BcEntities),
			gen_event:notify(EntitiesEventPid, {entity_moved, ReorientedBcEntity}),
			MoveSpeed = bc_entity_config:move_speed(BcEntityConfig),
			TimerRef = send_action_complete(MoveSpeed),
			{next_state, moving, State#state{entity = ReorientedBcEntity,
											 timer = TimerRef}};
		{error, _} ->
			sense(State)
	end.

send_action_complete(Speed) ->
	DelayFloat = 1000 - (1000 * Speed),
	DelayInt = erlang:trunc(DelayFloat),
	gen_fsm:send_event_after(DelayInt, action_complete).

move_entity(Direction, #state{entity = BcEntity, map = BcMap} = State) ->
	OriginalBcCollision = bc_entity:to_collision(BcEntity),
	UpdatedBcCollision = bc_collision:move(Direction, OriginalBcCollision),
	case bc_map:update_collision(BcMap, OriginalBcCollision, UpdatedBcCollision) of
		ok ->
			OrientedBcEntity =
				case bc_entity:orientation(BcEntity) =:= Direction of
					true -> BcEntity;
					false -> reorient_entity(Direction, BcEntity, BcEntities)
				end,
			UpdatedBcVertices = bc_collision:vertices(UpdatedBcCollision),
			MovedBcEntity = bc_entity:set_vertices(UpdatedBcVertices, OrientedBcEntity),
			{ok, MovedBcEntity};
		{error, _} = Error ->
			Error
	end.

reorient_entity(Orientation, BcEntity, BcEntities) ->
	UpdatedBcEntity = bc_entity:set_orientation(Orientation, BcEntity),
	bc_entities:insert(UpdatedBcEntity, BcEntities),
	UpdatedBcEntity.
