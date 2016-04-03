
-module(map_utils).
-export([are_neighbors/3]).

%%====================================================================
%% API functions
%%====================================================================

-spec are_neighbors(graph(), vertex(), vertex()) -> boolean().
are_neighbors(MapGraph, Vertex, Neighbor) ->
	Row = maps:get(row, Vertex),
	Col = maps:get(col, Vertex),
	InNeighbors = digraph:in_neighbours(MapGraph, Vertex),
	OutNeighbors = digraph:out_neighbours(MapGraph, Vertex),
	lists:any(fun(InNeighbor) -> 
				maps:get(row, InNeighbor) =:= Row andalso
				maps:get(col, InNeighbor) =:= Col
		   end, InNeighbors) andalso
	 lists:any(fun(OutNeighbor) -> 
					maps:get(row, OutNeighbor) =:= Row andalso
					maps:get(col, OutNeighbor) =:= Col
			    end, OutNeighbors).
	