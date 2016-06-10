
-module(bc_map).

%% api exports
-export([init/1, 
		 compute_path/3,
		 are_neighbors/3,
		 reaching_neighbors/3]).

%% vertex inside the graph
-type vertex() :: #{row => integer(),
					col => integer()}.

%%====================================================================
%% API functions
%%====================================================================

init(MapFile) ->
	%% MapFileStr = filename:join([code:priv_dir(battlecraft), "map", "map.json"]),
	{ok, Json} = file:read_file(MapFile),
	TmxJsonMap = jsx:decode(Json, [return_maps]),
	bc_tmx:load_graph(TmxJsonMap).

-spec compute_path(MapGraph :: digraph:graph(), 
				   Vertex1 :: vertex(), 
				   Vertex2 :: vertex()) -> [vertex()] | false.
compute_path(MapGraph, Vertex1, Vertex2) ->
	digraph:get_path(MapGraph, Vertex1, Vertex2).

-spec are_neighbors(MapGraph :: digraph:graph(), 
					Vertex :: vertex(), 
					Neighbor:: vertex()) -> boolean().
are_neighbors(MapGraph, Vertex, #{row := NeighborRow, col := NeighborCol}) ->
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

-spec reaching_neighbors(MapGraph :: digraph:graph(), 
						 Vertex :: vertex(), 
						 MaxDist :: integer()) -> [vertex()].
reaching_neighbors(MapGraph, Vertex, MaxDist) ->
	Neighbors = reaching_neighbors(MapGraph, [Vertex], MaxDist, []),
	lists:delete(Vertex, Neighbors).

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
