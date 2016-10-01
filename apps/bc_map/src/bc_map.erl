
-module(bc_map).
-include_lib("stdlib/include/qlc.hrl").

%% ets table options
-define(ETS_OPTIONS(Heir), [set, 
							public, 
							{heir, Heir, []}, 
							{write_concurrency, true}]).

%% api exports
-export([init/1,
		 init/2,
		 insert_collision/2,
		 base_vertices/2,
		 base1_vertices/1,
		 base2_vertices/1,
		 base3_vertices/1,
		 base4_vertices/1,
		 base_spawn_vertices/2,
		 base1_spawn_vertices/1,
		 base2_spawn_vertices/1,
		 base3_spawn_vertices/1,
		 base4_spawn_vertices/1,
		 query_collisions/2,
		 query_ids/2,
		 compute_path/3,
		 are_vertices/2,
		 are_neighbors/3,
		 reaching_neighbors/3,
		 update_collision/3, 
		 delete_collision/2]).

%%
%% @doc map for graph and collisions
%%
-type map_graph() :: #{graph => digraph:graph(), 
					   base_vertices => map(), 
					   coll_tab => ets:tid()}.

%%
%% @doc query result rows
%%
-type query_res() :: #{uiid => string(), 
					   vertex => bc_vertex:vertex()}.

%% type exports
-export_type([map_graph/0, query_res/0]).

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
	{ok, Dims} = bc_tmx:load_dims(TmxJsonMap),
	{ok, MapGraph} = bc_tmx:load_graph(TmxJsonMap, Dims),
	{ok, BaseVertices} = bc_tmx:load_base_collision_verticies(TmxJsonMap, MapGraph, Dims),
	{ok, SpawnVertices} = bc_tmx:load_base_spawn_vertices(TmxJsonMap, MapGraph, Dims),
	#{graph => MapGraph, 
	  base_vertices => BaseVertices, 
	  spawn_vertices => SpawnVertices, 
	  coll_tab => ets:new(collision, ?ETS_OPTIONS(Heir))}.

-spec insert_collision(MapGraph :: map_graph(),
					   BcCollision :: bc_collision:collision()) -> boolean().
insert_collision(MapGraph, BcCollision) ->
	Tab = maps:get(coll_tab, MapGraph),
	Rows = vertex_rows(BcCollision),
	ets:insert_new(Tab, Rows).

-spec base_vertices(MapGraph :: map_graph(), BaseNum :: integer()) -> [bc_vertex:vertex()].
base_vertices(MapGraph, BaseNum) ->
	case BaseNum of
		 1 -> base1_vertices(MapGraph);
		 2 -> base2_vertices(MapGraph);
		 3 -> base3_vertices(MapGraph);
		 4 -> base4_vertices(MapGraph)
	end.

-spec base1_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base1_vertices(MapGraph) ->
	BaseVertices = maps:get(base_vertices, MapGraph),
	maps:get(base1, BaseVertices).

-spec base2_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base2_vertices(MapGraph) ->
	BaseVertices = maps:get(base_vertices, MapGraph),
	maps:get(base2, BaseVertices).

-spec base3_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base3_vertices(MapGraph) ->
	BaseVertices = maps:get(base_vertices, MapGraph),
	maps:get(base3, BaseVertices).

-spec base4_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base4_vertices(MapGraph) ->
	BaseVertices = maps:get(base_vertices, MapGraph),
	maps:get(base4, BaseVertices).

-spec base_spawn_vertices(MapGraph :: map_graph(), BaseNum :: integer()) -> [bc_vertex:vertex()].
base_spawn_vertices(MapGraph, BaseNum) ->
	case BaseNum of
		1 -> base1_spawn_vertices(MapGraph);
		2 -> base2_spawn_vertices(MapGraph);
		3 -> base3_spawn_vertices(MapGraph);
		4 -> base4_spawn_vertices(MapGraph)
	end.

-spec base1_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base1_spawn_vertices(MapGraph) ->
	SpawnVertices = maps:get(spawn_vertices, MapGraph),
	maps:get(base1, SpawnVertices).

-spec base2_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base2_spawn_vertices(MapGraph) ->
	SpawnVertices = maps:get(spawn_vertices, MapGraph),
	maps:get(base2, SpawnVertices).

-spec base3_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base3_spawn_vertices(MapGraph) ->
	SpawnVertices = maps:get(spawn_vertices, MapGraph),
	maps:get(base3, SpawnVertices).

-spec base4_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base4_spawn_vertices(MapGraph) ->
	SpawnVertices = maps:get(spawn_vertices, MapGraph),
	maps:get(base4, SpawnVertices).

-spec query_collisions(MapGraph :: map_graph(),
					   BcVertices :: [bc_vertex:vertex()]) -> [query_res()].
query_collisions(#{coll_tab := Tab}, BcVertices) ->
	BcVertexTuples = lists:map(fun(BcVertex) -> bc_vertex:to_tuple(BcVertex) end, BcVertices),
	Results = qlc:eval(qlc:q([{BcVertexTuple, Uuid} || 
							  {BcVertexTuple, Uuid} <- ets:table(Tab),  
							  lists:member(BcVertexTuple, BcVertexTuples)])),
	rows_to_query_results(Results).

-spec query_ids(MapGraph :: map_graph(), 
				Uuids :: uuid:uuid() | [uuid:uuid()]) -> [query_res()].
query_ids(MapGraph, Uuids) when is_list(Uuids) ->
	Tab = maps:get(coll_tab, MapGraph),
	Results = qlc:eval(qlc:q([{BcVertexTuple, Uuid} || 
							  {BcVertexTuple, Uuid} <- ets:table(Tab),
							  lists:member(Uuid, Uuids)])),
	rows_to_query_results(Results);
query_ids(MapGraph, Uuid) ->
	query_ids(MapGraph, [Uuid]).

-spec compute_path(MapGraph :: map_graph(), 
				   Vertex1 :: bc_vertex:vertex(), 
				   Vertex2 :: bc_vertex:vertex()) -> [bc_vertex:vertex()] | false.
compute_path(MapGraph, Vertex1, Vertex2) ->
	Graph = maps:get(graph, MapGraph),
	digraph:get_short_path(Graph, Vertex1, Vertex2).

-spec are_vertices(MapGraph :: map_graph(),
				   BcVertices :: [bc_vertex:vertex()]) -> boolean().
are_vertices(MapGraph, BcVertices) ->
	Graph = maps:get(graph, MapGraph),
	DigraphBcVertices = digraph:vertices(Graph),
	lists:all(fun(BcVertex) -> 
				lists:member(BcVertex, DigraphBcVertices) 
			  end, BcVertices).

-spec are_neighbors(MapGraph :: map_graph(), 
					Vertex :: bc_vertex:vertex(), 
					Neighbor :: bc_vertex:vertex()) -> boolean().
are_neighbors(MapGraph, Vertex, Neighbor) ->
	Graph = maps:get(graph, MapGraph),
	NeighborRow = bc_vertex:row(Neighbor),
	NeighborCol = bc_vertex:col(Neighbor),
	InNeighbors = digraph:in_neighbours(Graph, Vertex),
	OutNeighbors = digraph:out_neighbours(Graph, Vertex),
	lists:any(fun(InNeighbor) -> 
				bc_vertex:row(InNeighbor) =:= NeighborRow andalso
				bc_vertex:col(InNeighbor) =:= NeighborCol
		   	end, InNeighbors) andalso
	 lists:any(fun(OutNeighbor) -> 
					bc_vertex:row(OutNeighbor) =:= NeighborRow andalso
					bc_vertex:col(OutNeighbor) =:= NeighborCol
			    end, OutNeighbors).

-spec reaching_neighbors(MapGraph :: map_graph(), 
						 BcVertices :: [bc_vertex:vertex()] | bc_vertex:vertex(), 
						 MaxDist :: integer()) -> [bc_vertex:vertex()].
reaching_neighbors(MapGraph, BcVertices, MaxDist) when is_list(BcVertices) ->
	Graph = maps:get(graph, MapGraph),
	Neighbors = reaching_neighbors(Graph, BcVertices, MaxDist, []),
	NeighborSet = sets:from_list(Neighbors),
	QuerySet = sets:from_list(BcVertices),
	ReachingNeighbors = sets:subtract(NeighborSet, QuerySet),
	sets:to_list(ReachingNeighbors);
reaching_neighbors(BcMap, BcVertex, MaxDist) ->
	reaching_neighbors(BcMap, [BcVertex], MaxDist).

-spec update_collision(MapGraph :: map_graph(),
					   OriginalBcCollision :: bc_collision:collision(),
					   UpdatedBcCollision :: bc_collision:collision()) -> ok | {error, Reason :: string()}.
update_collision(MapGraph, OriginalBcCollision, UpdatedBcCollision) ->
	Tab = maps:get(coll_tab, MapGraph),
	case bc_collision:difference_vertices(UpdatedBcCollision, OriginalBcCollision) of
		InsertBcVertices when length(InsertBcVertices) > 0 ->
			Uuid = bc_collision:uuid(UpdatedBcCollision),
			InsertRows = vertex_rows(Uuid, InsertBcVertices),
			case ets:insert_new(Tab, InsertRows) of
				true ->
					DeleteBcVertices = bc_collision:difference_vertices(
									   OriginalBcCollision, UpdatedBcCollision),
					Ms = collision_ms(DeleteBcVertices),
					ets:select_delete(Tab, Ms),
					ok;
				false ->
					{error, "Unable to insert new vertices."}
			end;
		_ ->
			{error, "Updated collision doesn't have different vertices."}
	end.

-spec delete_collision(MapGraph :: map_graph(),
					   BcCollision :: bc_collision:collision()) -> boolean().
delete_collision(MapGraph, BcCollision) ->
	Tab = maps:get(coll_tab, MapGraph),
	Ms = collision_ms(BcCollision),
	ets:select_delete(Tab, Ms) > 0.

%%====================================================================
%% Internal functions
%%====================================================================

rows_to_query_results(Rows) ->
	lists:map(fun({{Row, Col}, Uuid}) -> #{uuid => Uuid, 
										   vertex => bc_vertex:init(Row, Col)} end, Rows).

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

vertex_rows(BcCollision) when erlang:is_map(BcCollision) ->
	Uuid = bc_collision:uuid(BcCollision),
	BcVertices = bc_collision:vertices(BcCollision),
	vertex_rows(Uuid, BcVertices).

vertex_rows(Uuid, BcVertices) when erlang:is_list(BcVertices) ->
	lists:map(fun(BcVertex) -> 
				Row = bc_vertex:row(BcVertex), 
				Col = bc_vertex:col(BcVertex), 
				{{Row, Col}, Uuid} 
			  end, BcVertices).

collision_ms(BcCollision) when is_map(BcCollision) ->
	BcVertices = bc_collision:vertices(BcCollision),
	collision_ms(BcVertices);
collision_ms(BcVertices) when is_list(BcVertices) ->
	[{{{'$1','$2'},'_'},
	  [{'andalso',{'==','$1',Row},
		{'==','$2',Col}}],[true]} || 
	 	#{row := Row, col := Col} <- BcVertices].
