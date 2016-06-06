
-module(bc_map_utils).

%% API exports
-export([are_neighbors/3,
		 tile_inside_collision/4, 
		 inside_collision/4,
		 collision_verticies]).

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
tile_inside_collision(DimMap, CollisionMap, Row, Col) ->
	MinY = maps:get(y, CollisionMap),
	MinX = maps:get(x, CollisionMap),
	MaxY = MinY + maps:get(height, CollisionMap),
	MaxX = MinX + maps:get(width, CollisionMap),
	PosList = tile_endpoints(DimMap, Row, Col),
	lists:any(fun(Pos) ->
					Y = maps:get(y, Pos),
					X = maps:get(x, Pos),
					Y >= MinY andalso Y =< MaxY andalso 
					X >= MinX andalso X =< MaxX
				end, PosList).

%% @spec inside_collision(dims(), [collision()], integer(), integer()) -> boolean().
inside_collision(DimMap, CollisionMapList, Row, Col) ->
	lists:any(fun(CollisionMap) -> 
					  tile_inside_collision(DimMap, CollisionMap, Row, Col) 
			  end, CollisionMapList).

collision_verticies(MapGraph, Dims, RawCollisionMaps) ->
	collision_verticies(MapGraph, Dims, RawCollisionMaps, []).

%%====================================================================
%% Internal functions
%%====================================================================

collision_verticies(_, _, [], Verticies) ->
	Verticies;
collision_verticies(MapGraph, Dims, [RawCollisionMap|RawCollisionMaps], Verticies) ->
	VerticiesAcc = Verticies ++ lists:filter(fun(Vertex) -> 
													Row = maps:get(row, Vertex),
													Col = maps:get(col, Vertex),
													tile_inside_collision(Dims, RawCollisionMap, Row, Col)
												end, digraph:vertices(MapGraph)),
	collision_verticies(MapGraph, Dims, RawCollisionMaps, VerticiesAcc).

tile_endpoints(Dims, Row, Col) ->
	TileHeight = maps:get(tileheight, Dims),
	TileWidth = maps:get(tilewidth, Dims),
	[		
	   #{y => Row * TileHeight, x => Col * TileWidth},
	   #{y => Row * TileHeight, x => (Col + 1) * TileWidth},
	   #{y => (Row + 1) * TileHeight, x => (Col + 1) * TileWidth},
	   #{y => (Row + 1) * TileHeight, x => Col * TileWidth}
	].
	