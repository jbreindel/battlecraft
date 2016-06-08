
-module(bc_collision).

%% ets table options
-define(ETS_OPTIONS(BcGameSup), [set, 
								 public, 
								 {heir, BcGameSup, []}, 
								 {write_concurrency, true}]).

-spec create(BcGameSup :: pid(), 
			 GameId :: integer()) -> ets:tid().
create(BcGameSup) ->
	ets:new(collision, ?ETS_OPTIONS(BcGameSup)).

-spec insert(Tab :: ets:tid(),
			 CollisionMap :: bc_collision_serv:collision()) -> true.
insert(Tab, CollisionMap) ->
	Rows = vertex_rows(CollisionMap),
	ets:insert(Tab, Rows).

-spec insert_new(Tab :: ets:tid(),
				 CollisionMap :: bc_collision_serv:collision()) -> boolean().
insert_new(Tab, CollisionMap) ->
	Rows = vertex_rows(CollisionMap),
	ets:insert_new(Tab, Rows).

%% update(Tab, OriginalCollisionMap, UpdatedCollisionMap) ->
%% 	
	
vertex_rows(#{id := Id, vertices := Vertices} = CollisionMap) ->
	lists:map(fun(Vertex) -> {Vertex, Id} end, Vertices).													  
	