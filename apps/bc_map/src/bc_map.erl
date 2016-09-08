
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
		 base1_vertices/1,
		 base1_spawn_vertices/1,
		 base2_vertices/1,
		 base2_spawn_vertices/1,
		 base3_vertices/1,
		 base3_spawn_vertices/1,
		 base4_vertices/1,
		 base4_spawn_vertices/1,
		 query_collisions/2,
		 query_ids/2,
		 compute_path/3,
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
insert_collision(#{coll_tab := Tab}, BcCollision) ->
	Rows = vertex_rows(BcCollision),
	ets:insert_new(Tab, Rows).

-spec base1_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base1_vertices(#{base_vertices := BaseVertices}) ->
	maps:get(base1, BaseVertices).

-spec base1_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base1_spawn_vertices(#{spawn_vertices := SpawnVertices}) ->
	maps:get(base1, SpawnVertices).

-spec base2_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base2_vertices(#{base_vertices := BaseVertices}) ->
	maps:get(base2, BaseVertices).

-spec base2_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base2_spawn_vertices(#{spawn_vertices := SpawnVertices}) ->
	maps:get(base2, SpawnVertices).

-spec base3_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base3_vertices(#{base_vertices := BaseVertices}) ->
	maps:get(base3, BaseVertices).

-spec base3_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base3_spawn_vertices(#{spawn_vertices := SpawnVertices}) ->
	maps:get(base3, SpawnVertices).

-spec base4_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base4_vertices(#{base_vertices := BaseVertices}) ->
	maps:get(base4, BaseVertices).

-spec base4_spawn_vertices(MapGraph :: map_graph()) -> [bc_vertex:vertex()].
base4_spawn_vertices(#{spawn_vertices := SpawnVertices}) ->
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
query_ids(#{coll_tab := Tab}, Uuids) when is_list(Uuids) ->
	Results = qlc:eval(qlc:q([{BcVertexTuple, Uuid} || 
							  {BcVertexTuple, Uuid} <- ets:table(Tab),
							  lists:member(Uuid, Uuids)])),
	rows_to_query_results(Results);
query_ids(MapGraph, Uuid) ->
	query_ids(MapGraph, [Uuid]).

-spec compute_path(MapGraph :: map_graph(), 
				   Vertex1 :: bc_vertex:vertex(), 
				   Vertex2 :: bc_vertex:vertex()) -> [bc_vertex:vertex()] | false.
compute_path(#{graph := MapGraph}, Vertex1, Vertex2) ->
	digraph:get_path(MapGraph, Vertex1, Vertex2).

-spec are_neighbors(MapGraph :: map_graph(), 
					Vertex :: bc_vertex:vertex(), 
					Neighbor:: bc_vertex:vertex()) -> boolean().
are_neighbors(#{graph := MapGraph}, Vertex, Neighbor) ->
	NeighborRow = bc_vertex:row(Neighbor),
	NeighborCol = bc_vertex:col(Neighbor),
	InNeighbors = digraph:in_neighbours(MapGraph, Vertex),
	OutNeighbors = digraph:out_neighbours(MapGraph, Vertex),
	lists:any(fun(InNeighbor) -> 
				bc_vertex:row(InNeighbor) =:= NeighborRow andalso
				bc_vertex:col(InNeighbor) =:= NeighborCol
		   	end, InNeighbors) andalso
	 lists:any(fun(OutNeighbor) -> 
					bc_vertex:row(OutNeighbor) =:= NeighborRow andalso
					bc_vertex:col(OutNeighbor) =:= NeighborCol
			    end, OutNeighbors).

-spec reaching_neighbors(MapGraph :: map_graph(), 
						 Vertex :: bc_vertex:vertex(), 
						 MaxDist :: integer()) -> [bc_vertex:vertex()].
reaching_neighbors(#{graph := MapGraph}, Vertex, MaxDist) ->
	Neighbors = reaching_neighbors(MapGraph, [Vertex], MaxDist, []),
	lists:delete(Vertex, Neighbors).

-spec update_collision(MapGraph :: map_graph(),
					   OriginalBcCollision :: bc_collision:collision(),
					   UpdatedBcCollision :: bc_collision:collision()) -> ok | {error, Reason :: string()}.
update_collision(#{coll_tab := Tab}, OriginalBcCollision, UpdatedBcCollision) ->
	case bc_collision:difference_vertices(UpdatedBcCollision, OriginalBcCollision) of
		InsertBcVertices when length(InsertBcVertices) > 0 ->
			Uuid = bc_collision:uuid(UpdatedBcCollision),
			InsertRows = vertex_rows(Uuid, InsertBcVertices),
			case ets:insert_new(Tab, InsertRows) of
				true ->
					DeleteBcVertices = bc_collision:difference_vertices(
									   OriginalBcCollision, UpdatedBcCollision),
					Ms = collision_ms(DeleteBcVertices),
					ets:match_delete(Tab, Ms),
					ok;
				false ->
					{error, "Unable to insert new vertices."}
			end;
		_ ->
			{error, "Updated collision doesn't have different vertices."}
	end.

-spec delete_collision(MapGraph :: map_graph(),
					   BcCollision :: bc_collision:collision()) -> true.
delete_collision(#{coll_tab := Tab}, BcCollision) ->
	Ms = collision_ms(BcCollision),
	ets:match_delete(Tab, Ms).

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
	  [{'andalso',{'=:=','$1',Row},
		{'=:=','$2',Col}}],['$_']} || #{row := Row, col := Col} <- BcVertices].
