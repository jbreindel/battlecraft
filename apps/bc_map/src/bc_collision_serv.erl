
-module(bc_collision_serv).
-behavior(gen_server).

%% api functions
-export([start_link/0, 
		 add_collision/1, 
		 update_collision/1, 
		 query_collision/1, 
		 query_positions/1, 
		 remove_collision/1
		]).

%% gen_server callbacks
-export(init/1, 
		handle_call/3, 
		handle_cast/2).

%% public types
-export_type([collision/0]).

%% collision 
-type collision() :: #{id => string(),
					   vertices => [bc_map_serv:vertex()]}.

%% internal state
-record(state, {coll_tab_id}).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
	gen_server:start_link(?MODULE, Name, [], []).

add_collision(BcCollisionServ, CollisionMap) ->
	gen_server:call(bc_collision_serv, {add_collision, CollisionMap}).

update_collision(BcCollisionServ, CollisionMap) ->
	gen_server:call(BcCollisionServ, {update_collision, CollisionMap}).

query_collision(BcCollisionServ, Id) ->
	gen_server:call(BcCollisionServ, {query_collision, Id}).

query_positions(BcCollisionServ, Verticies) when erlang:is_list(Vertices) ->
	gen_server:call(bc_collision_serv, {query_positions, Verticies});
query_positions(BcCollisionServ, Vertex) when erlang:is_map(Vertex) ->
	gen_server:call(bc_collision_serv, {query_positions, [Vertex]}).

remove_collision(BcCollisionServ, Id) ->
	gen_server:cast(BcCollisionServ, {remove_collection, Id}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

init(BcGameSup) ->
	CollTabId = ets:new(collisions, [bag, protected, {heir, BcGameSup}]),
	{ok, #state{coll_tab_id = CollTabId}}.

handle_call({add_collision, CollisionMap}, _From, #state{coll_tab_id = CollTabId} = State) ->
	Id = maps:get(id, CollisionMap),
	Vertices = maps:get(vertices, CollisionMap);
	%% TODO insert into ets

handle_call({update_collision, CollisionMap}, _From, State) ->
	#state{collisions = CollisionOrddict} = State,
	case maps:get(verticies, CollisionMap) of
		undefined ->
			{reply, {error, "Bad request"}};
		Verticies ->
			QueryCollisionList = query_collisions(CollisionOrddict, Verticies),
			case length(QueryCollisionList) of
				0 ->
					Id = maps:get(id, CollisionMap),
					try ordict:update(Id, fun(_) -> CollisionMap end, CollisionOrddict) of
						UpdatedCollisionOrddict ->
							{reply, ok, #state{collisions = UpdatedCollisionOrddict}}
					catch
						_ ->
							{reply, {error, "Unable to update collision."}, State}
					end;
				_ ->
					{reply, {occupied, QueryCollisionList}}
			end
	end.

handle_call({query_collision, Id}, _From, State) ->
	#state{collisions = CollisionOrddict} = State,
	try orddict:find(Id, CollisionOrddict) of
		{ok, CollisionMap} ->
			{reply, {ok, CollisionMap}, State};
		error ->
			{reply, {not_found, "Collision could not be found"}, State}
	catch
		_ ->
			{reply, {error, "Cannot process query for id."}}
	end;

handle_call({query_positions, Verticies}, _From, State) ->
	#state{collisions = CollisionOrddict} = State,
	try query_collisions(CollisionOrddict, Verticies) of
		ResultCollisionList ->
			{reply, {ok, ResultCollisionList}, State}
	catch
		_ ->
			{reply, {error, "Unable to query for verticies"}}
	end.

handle_cast({remove_collision, Id}, State) ->
	#state{collisions = CollisionOrddict} = State,
	try orddict:erase(Id, CollisionOrddict) of
		UpdatedCollisionOrddict ->
			{noreply, #state{collisions = UpdatedCollisionOrddict}}
	catch
		_ ->
			{noreply, State}
	end.

%%====================================================================
%% Internal Functions
%%====================================================================

query_collisions(CollisionOrddict, Verticies) ->
	QueryResult = orddict:filter(fun(Key, Value) ->  
									CollVertexList = maps:get(verticies, Value),
									lists:any(fun(CollVertex) ->  
												lists:any(fun(QueryVertex) ->  
																maps:get(row, QueryVertex) =:= maps:get(row, CollVertex) andalso
																maps:get(col, QueryVertex) =:= maps:get(col, CollVertex)
															end, Verticies)
												end, CollVertexList)
									end, CollisionOrddict),
	orddict:to_list(QueryResult).
