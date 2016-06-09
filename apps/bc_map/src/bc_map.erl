
-module(bc_map).

%% api exports
-export([init/1, 
		 compute_path/2,
		 are_neighbors/2]).

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
					Vertex :: bc_map_serv:vertex(), 
					Neighbor:: bc_map_serv:vertex()) -> boolean().
are_neighbors(MapGraph, #{row := VertexRow, col := VertexCol} = Vertex, 
			  #{row := NeighborRow, col := NeighborCol} = Neighbor) ->
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

