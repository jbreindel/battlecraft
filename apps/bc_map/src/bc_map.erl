
-module(bc_map).

%% ets table options
-define(ETS_OPTIONS(Heir), [set, 
							public, 
							{heir, Heir, []}, 
							{write_concurrency, true}]).

%% api exports
-export([init/1,
		 init/2,
		 insert_collision/2, 
		 query_collisions/2,
		 compute_path/3,
		 are_neighbors/3,
		 reaching_neighbors/3,
		 update_collision/3, 
		 delete_collision/2]).

%% vertex inside the graph
-type vertex() :: #{row => integer(),
					col => integer()}.

%%
%% @doc map for graph and collisions
%%
-type map_graph() :: #{graph => digraph:graph(), coll_tab => ets:tid()}.

%%
%% @doc collision map
%%
-type collision() :: #{id => string(), vertices => [bc_map:vertex()]}.

%%
%% @doc query result rows
%%
-type query_res() :: #{id => string(), vertex => bc_map:vertex()}.

%% type exports
-export_type([vertex/0, map_graph/0, collision/0, query_res/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec init(Heir :: pid()) -> map_graph().
init(Heir) ->
	MapFile = filename:join([code:priv_dir(bc_map), "map.json"]),
	init(Heir, MapFile).

-spec init(Heir :: pid(), MapFile :: string()) -> map_graph().
init(Heir, MapFile) ->
	{ok, Json} = file:read_file(MapFile),
	TmxJsonMap = jsx:decode(Json, [return_maps]),
	{ok, MapGraph} = bc_tmx:load_graph(TmxJsonMap),
	#{graph => MapGraph, 
	  coll_tab => ets:new(collision, ?ETS_OPTIONS(Heir))}.

-spec insert_collision(MapGraph :: map_graph(),
					   CollisionMap :: collision()) -> boolean().
insert_collision(#{coll_tab := Tab}, CollisionMap) ->
	Rows = vertex_rows(CollisionMap),
	ets:insert_new(Tab, Rows).

-spec query_collisions(MapGraph :: map_graph(),
					   Vertices :: [bc_map:vertex()]) -> [query_res()].
query_collisions(#{coll_tab := Tab}, Vertices) ->
	Ms = collision_ms(Vertices),
	case ets:select(Tab, Ms) of
		Results when is_list(Results) ->
			lists:map(fun({{Row, Col}, Id}) -> #{id => Id, 
												 vertex => #{row => Row, 
															 col => Col}} end, Results);
		_ ->
			{error, "Unable to query collisions."}
	end.

-spec compute_path(MapGraph :: map_graph(), 
				   Vertex1 :: vertex(), 
				   Vertex2 :: vertex()) -> [vertex()] | false.
compute_path(#{graph := MapGraph}, Vertex1, Vertex2) ->
	digraph:get_path(MapGraph, Vertex1, Vertex2).

-spec are_neighbors(MapGraph :: map_graph(), 
					Vertex :: vertex(), 
					Neighbor:: vertex()) -> boolean().
are_neighbors(#{graph := MapGraph}, Vertex, #{row := NeighborRow, col := NeighborCol}) ->
	InNeighbors = digraph:in_neighbours(MapGraph, Vertex),
	OutNeighbors = digraph:out_neighbours(MapGraph, Vertex),
	lists:any(fun(InNeighbor) -> 
				maps:get(row, InNeighbor) =:= NeighborRow andalso
				maps:get(col, InNeighbor) =:= NeighborCol
		   	end, InNeighbors) andalso
	 lists:any(fun(OutNeighbor) -> 
					maps:get(row, OutNeighbor) =:= NeighborRow andalso
					maps:get(col, OutNeighbor) =:= NeighborCol
			    end, OutNeighbors).

-spec reaching_neighbors(MapGraph :: map_graph(), 
						 Vertex :: vertex(), 
						 MaxDist :: integer()) -> [vertex()].
reaching_neighbors(#{graph := MapGraph}, Vertex, MaxDist) ->
	Neighbors = reaching_neighbors(MapGraph, [Vertex], MaxDist, []),
	lists:delete(Vertex, Neighbors).

-spec update_collision(MapGraph :: map_graph(),
					   OriginalCollisionMap :: collision(),
					   UpdatedCollisionMap :: collision()) -> ok | {error, Reason :: string()}.
update_collision(#{coll_tab := Tab}, OriginalCollisionMap, UpdatedCollisionMap) ->
	case difference_vertices(UpdatedCollisionMap, OriginalCollisionMap) of
		InsertRows when length(InsertRows) > 0 ->
			case ets:insert_new(Tab, InsertRows) of
				true ->
					DifferenceRows = difference_vertices(OriginalCollisionMap, UpdatedCollisionMap),
					Ms = collision_ms(DifferenceRows),
					ets:match_delete(Tab, Ms),
					ok;
				false ->
					{error, "Unable to insert new vertices."}
			end;
		_ ->
			{error, "Updated collision doesn't have different vertices."}
	end.

-spec delete_collision(MapGraph :: map_graph(),
					   CollisionMap :: collision()) -> true.
delete_collision(#{coll_tab := Tab}, CollisionMap) ->
	Ms = collision_ms(CollisionMap),
	ets:match_delete(Tab, Ms).

%%====================================================================
%% Internal functions
%%====================================================================

reaching_neighbors(_, _, MaxDist, NeighborAcc) when MaxDist =:= 0 ->
	NeighborSet = sets:from_list(NeighborAcc),
	sets:to_list(NeighborSet);
reaching_neighbors(MapGraph, Vertices, MaxDist, NeighborAcc) ->
	NeighborLists = lists:filtermap(fun(V) -> 
										case digraph:out_neighbours(MapGraph, V) of
											OutNeighbors when length(OutNeighbors) > 0 ->
												{true, OutNeighbors};
											_ ->
												false
										end
									end, Vertices),
	Neighbors = lists:flatten(NeighborLists),
	reaching_neighbors(MapGraph, Neighbors, MaxDist -1, NeighborAcc ++ Neighbors).

vertex_rows(#{id := Id, vertices := Vertices} = CollisionMap) when erlang:is_map(CollisionMap) ->
	vertex_rows(Id, Vertices).

vertex_rows(Id, Vertices) when erlang:is_list(Vertices) ->
	lists:map(fun(#{row := Row, col := Col}) -> {{Row, Col}, Id} end, Vertices).

difference_vertices(#{vertices := Vertices1} = CollisionMap1,
				    #{vertices := Vertices2} = CollisionMap2) ->
	Set1 = sets:from_list(Vertices1),
	Set2 = sets:from_list(Vertices2),
	DiffSet = sofs:difference(Set1, Set2),
	sets:to_list(DiffSet).

collision_ms(#{vertices := Vertices} = CollisionMap) when is_map(CollisionMap) ->
	collision_ms(Vertices);
collision_ms(Vertices) when is_list(Vertices) ->
	[{{{'$1','$2'},'_'},
	  [{'andalso',{'=:=','$1',Row},
		{'=:=','$2',Col}}],['$_']} || #{row := Row, col := Col} <- Vertices].
