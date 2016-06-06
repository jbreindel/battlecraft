
-module(bc_map_utils).

%% API exports
-export([are_neighbors/3,
		 tile_inside_object/4, 
		 inside_object/4,
		 object_verticies/3]).

%%====================================================================
%% API functions
%%====================================================================

%% @spec are_neighbors(graph(), vertex(), vertex()) -> boolean().
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

%% @spec tile_inside_collision(dims(), collision(), integer(), integer()) -> boolean().
tile_inside_object(DimMap, ObjectMap, Row, Col) ->
	MinY = maps:get(y, ObjectMap),
	MinX = maps:get(x, ObjectMap),
	MaxY = MinY + maps:get(height, ObjectMap),
	MaxX = MinX + maps:get(width, ObjectMap),
	PosList = tile_endpoints(DimMap, Row, Col),
	lists:any(fun(Pos) ->
					Y = maps:get(y, Pos),
					X = maps:get(x, Pos),
					Y >= MinY andalso Y =< MaxY andalso 
					X >= MinX andalso X =< MaxX
				end, PosList).

%% @spec inside_collision(dims(), [collision()], integer(), integer()) -> boolean().
inside_object(DimMap, ObjectMapList, Row, Col) ->
	lists:any(fun(ObjectMap) -> 
					  tile_inside_object(DimMap, ObjectMap, Row, Col) 
			  end, ObjectMapList).

object_verticies(MapGraph, Dims, ObjectMapList) ->
	object_verticies(MapGraph, Dims, ObjectMapList, []).

%%====================================================================
%% Internal functions
%%====================================================================

object_verticies(_, _, [], Verticies) ->
	Verticies;
object_verticies(MapGraph, Dims, [ObjectMap|ObjectMaps], Verticies) ->
	VerticiesAcc = Verticies ++ lists:filter(fun(Vertex) ->
												Row = maps:get(row, Vertex),
												Col = maps:get(col, Vertex),
												tile_inside_object(Dims, ObjectMap, Row, Col)
											end, digraph:vertices(MapGraph)),
	object_verticies(MapGraph, Dims, ObjectMaps, VerticiesAcc).

tile_endpoints(Dims, Row, Col) ->
	TileHeight = maps:get(tileheight, Dims),
	TileWidth = maps:get(tilewidth, Dims),
	[		
	   #{y => Row * TileHeight, x => Col * TileWidth},
	   #{y => Row * TileHeight, x => (Col + 1) * TileWidth},
	   #{y => (Row + 1) * TileHeight, x => (Col + 1) * TileWidth},
	   #{y => (Row + 1) * TileHeight, x => Col * TileWidth}
	].
	