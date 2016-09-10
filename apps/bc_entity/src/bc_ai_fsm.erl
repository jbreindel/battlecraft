
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

-export([start_link/3]).

%% internal state
-record(state, {entity,
				entity_config,
				entities,
				player_num,
				map,
				timer}).

%% ====================================================================
%% API functions
%% ====================================================================

-spec start_link(PlayerNum :: integer(),
				 BcEntity :: bc_entity:entity(), 
				 BcEntities :: bc_entities:entities(), 
				 BcMap :: bc_map:map_graph()) -> gen:start_ret().
start_link(PlayerNum, BcEntity, BcEntities, BcMap) ->
	gen_fsm:start_link(?MODULE, [PlayerNum, BcEntity, BcEntities, BcMap], []).

%% ====================================================================
%% Behavioural functions
%% ====================================================================

init([PlayerNum, BcEntity, BcEntities, BcMap]) ->
	UpdatedBcEntity = bc_entity:set_ai_fsm(self(), BcEntity),
	EntityType = bc_entity:entity_type(UpdatedBcEntity),
	{ok, BcEntityConfig} = bc_entities:entity_config(EntityType, BcEntities),
	StateName = case bc_entity_config:entity_class(BcEntityConfig) of 
					structure -> no_action; 
					_ -> standing 
				end,
	TimerRef = gen_fsm:send_event_after(5, action_complete),
	{ok, StateName, #state{entity = UpdatedBcEntity,
						   entity_config = BcEntityConfig, 
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
    ok.

code_change(OldVsn, StateName, StateData, Extra) ->
    {ok, StateName, StateData}.

%% ====================================================================
%% Internal functions
%% ====================================================================

sense(#state{bc_entity = BcEntity,
			 bc_entity_config = BcEntityConfig} = State) ->
	NearbyDistEntities = dist_entities(State),
	case lists:filter(fun({_, NearbyBcEntity}) -> 
						bc_entity:team(NearbyBcEntity) /= 
							bc_entity:team(BcEntity)
					 end, NearbyDistEntities) of
		NearbyEnemyEntities when length(NearbyEnemyEntities) > 0 ->
			SortedNearbyEnemyEntities = 
				lists:sort(fun({Dist1, _}, 
							   {Dist2, _}) -> 
								   Dist1 =< Dist2 
						   end, NearbyEnemyEntities),
			Range = bc_entity_config:range(BcEntityConfig),
			case lists:filtermap(fun({Dist, EnemyBcEntity}) -> 
									case Dist =< Range of 
										true -> {true, EnemyBcEntity};
										false -> false 
									end 
								end, SortedNearbyEnemyEntities) of
				InRangeEnemyBcEntities when length(InRangeEnemyBcEntities) > 0 ->
					%% attack in range enemies
					attack_entities(InRangeEnemyBcEntities, State);
				_ ->
					%% TODO move to closest reachable enemy to attack
					attack_entities()
			end;
		_ ->
			%% there are no nearby enemies, move forward
			
	end.

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
	end
		
%% attack_entity(InRangeEnemyBcEntity, State) ->
%% 	

dist_entities(#state{entity = BcEntity} = State) ->
	NearbyBcEntities = nearby_entities(State),
	lists:map(fun(NearbyBcEntity) ->
				Distance = bc_entity_util:distance(BcEntity, NearbyBcEntity),
				{Dstaince, NearbyBcEntity}
			  end, NearbyBcEntities).

nearby_entities(#state{entity = BcEntity, 
					   entities = BcEntities, 
					   map = BcMap} = State) ->
	EntityBcVertices = bc_entity:vertices(BcEntity),
	NeighborBcVertices = bc_map:reaching_neighbors(
						   BcMap, EntityBcVertices, ?SENSE_DIST),
	QueryRes = bc_map:query_collisions(BcMap, NeighborBcVertices),
	UuidSet = lists:foldl(fun(#{uuid := Uuid}, Set) -> 
						  	  sets:add_element(Uuid, Set) 
						  end, sets:new(), QueryRes),
	UuidList = sets:to_list(UuidSet),
	QueryRes = bc_entities:query(UuidList, BcEntities),
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
					end, QueryRes),	
				bc_entity:set_vertices(BcVertices, BcEntity)
			end, QueryRes).

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
		{ok, UpdatedBcEntity} ->
			EntitiesEventPid = bc_entities:event(BcEntities),
			gen_event:notify(EntitiesEventPid, {entity_moved, UpdatedBcEntity}),
			MoveSpeed = bc_entity_config:move_speed(BcEntityConfig),
			MoveDelayFloat = 1000 - (1000 * MoveSpeed),
			MoveDelayInt = erlang:trunc(MoveDelayFloat),
			TimerRef = gen_fsm:send_event_after(MoveDelayInt, action_complete),
			{next_state, moving, State#state{entity = UpdatedBcEntity,
											 timer = TimerRef}};
		{error, _} ->
			sense(State)
	end.

move_entity(Direction, #state{entity = BcEntity, map = BcMap} = State) ->
	OriginalBcCollision = bc_entity:to_collision(BcEntity),
	UpdatedBcCollision = bc_collision:move(Direction, OriginalBcCollision),
	case bc_map:update_collision(BcMap, OriginalBcCollision, UpdatedBcCollision) of
		ok ->
			UpdatedBcVertices = bc_collision:vertices(UpdatedBcCollision),
			UpdatedBcEntity = bc_entity:set_vertices(UpdatedBcVertices, BcEntity),
			{ok, UpdatedBcEntity};
		{error, _} = Error ->
			Error
	end.
