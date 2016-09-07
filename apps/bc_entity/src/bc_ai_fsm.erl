
-module(bc_ai_fsm).
-behaviour(gen_fsm).

-export([init/1, 
		 no_action/2,
		 standing/2,
		 moving/2, 
		 
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
	TimerRef = gen_fsm:send_event_after(5, action_complete),
	{ok, StateName, #state{entity = UpdatedBcEntity,
						   entity_config = BcEntityConfig, 
					   	   entities = BcEntities, 
					   	   map = BcMap,
						   timer = TimerRef}}.

no_action(action_complete, State) ->
	{next_state, no_action, State}.

standing(action_complete, State) ->
	%% TODO sense
	move(down, State).

moving(action_complete, State) ->
	%% TODO sense
	TimerRef = gen_fsm:send_event_after(0, action_complete),
	{next_state, standing, State#state{timer = TimerRef}}.

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

sense(State) ->
	%% TODO implement sense phase
	{next_state, standing, State}.

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
