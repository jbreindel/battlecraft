
-module(bc_tmx).

%% API exports
-export([load_graph/1,
		 load_dims/1,
		 load_object_list/1,
		 load_base_collision_verticies/1
		]).

%% type exports
-export_types([dims/0, object/0]).

-type dims() :: #{height => integer(),
				  width => integer(),
				  tileheight => integer(),
				  tilewidth => integer()}.

-type object() :: #{id => integer(),
					name => string(),
					x => integer(),
					y => integer(),
					height => integer(),
					width => integer()}.

%%====================================================================
%% API functions
%%====================================================================

-spec load_graph(map()) -> {ok, digraph:graph()} | {error, string()}.
load_graph(TmxJsonMap) ->
	{ok, process_map(TmxJsonMap)}.

-spec load_dims(map()) -> {ok, dims()} | {error, string()}.
load_dims(TmxJsonMap) ->
	{ok, process_dims(TmxJsonMap)}.

-spec load_object_list(map()) -> {ok, [object()]} | {error, string()}.
load_object_list(TmxJsonMap) ->
	ObjectMaps = process_objects(TmxJsonMap),
	{ok, lists:flatten(ObjectMaps)}.

-spec load_base_collision_verticies(map()) -> {ok, [object()]} | {error, string()}.
load_base_collision_verticies(TmxJsonMap) ->
	{ok, process_base_collisions(TmxJsonMap)}.

%%====================================================================
%% Internal functions
%%====================================================================

tile_endpoints(Dims, Row, Col) ->
	TileHeight = maps:get(tileheight, Dims),
	TileWidth = maps:get(tilewidth, Dims),
	[		
	   #{y => Row * TileHeight, x => Col * TileWidth},
	   #{y => Row * TileHeight, x => (Col + 1) * TileWidth},
	   #{y => (Row + 1) * TileHeight, x => (Col + 1) * TileWidth},
	   #{y => (Row + 1) * TileHeight, x => Col * TileWidth}
	].

-spec tile_inside_object(DimMap :: bc_tmx:dims(), 
						 ObjectMap :: bc_tmx:object(), 
						 Row :: integer(), 
						 Col :: integer()) -> boolean().
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

-spec inside_object(DimMap :: bc_tmx:dims(), 
					   ObjectMapList :: [bc_tmx:object()], 
					   Row :: integer(), 
					   Col :: integer()) -> boolean().
inside_object(DimMap, ObjectMapList, Row, Col) ->
	lists:any(fun(ObjectMap) -> 
					  tile_inside_object(DimMap, ObjectMap, Row, Col) 
			  end, ObjectMapList).

-spec object_verticies(MapGraph :: digraph:graph(), 
					   DimMap :: bc_tmx:dims(), 
					   ObjectMapList :: [bc_tmx:object()]) -> [bc_vertex:vertex()].
object_verticies(MapGraph, DimMap, ObjectMapList) ->
	object_verticies(MapGraph, DimMap, ObjectMapList, []).

object_verticies(_, _, [], Verticies) ->
	Verticies;
object_verticies(MapGraph, Dims, [ObjectMap|ObjectMaps], Verticies) ->
	VerticiesAcc = Verticies ++ lists:filter(fun(Vertex) ->
												Row = bc_vertex:row(Vertex),
												Col = bc_vertex:col(Vertex),
												tile_inside_object(Dims, ObjectMap, Row, Col)
											end, digraph:vertices(MapGraph)),
	object_verticies(MapGraph, Dims, ObjectMaps, VerticiesAcc).

object_layer(ObjectLayerMap) when erlang:is_map(ObjectLayerMap) ->
	case maps:get(<<"objects">>, ObjectLayerMap, undefined) of
		ObjectList when erlang:is_list(ObjectList) ->
			{true, lists:map(fun(ObjectMap) -> 
								#{
									id => maps:get(<<"id">>, ObjectMap, 0),
									name => binary:bin_to_list(maps:get(<<"name">>, ObjectMap)),
									x => maps:get(<<"x">>, ObjectMap, 0),
									y => maps:get(<<"y">>, ObjectMap, 0),
									height => maps:get(<<"height">>, ObjectMap, 0),
									width => maps:get(<<"width">>, ObjectMap, 0)
								}
							 end, ObjectList)};
		_ ->
			false
	end;
object_layer(_) ->
	false.

objects(TmxJsonMap, Filter) ->
	case maps:get(<<"layers">>, TmxJsonMap) of
		LayerList when erlang:is_list(LayerList) ->
			ObjectLayer = lists:filter(fun(LayerMap) ->
										   case maps:get(<<"name">>, LayerMap, undefined) of
											   undefined ->
												   false;
											   BinLayerName ->
												   LayerName = binary:bin_to_list(BinLayerName),
												   Filter(LayerName)
										   end
										end, LayerList),
			ObjectMaps = lists:filtermap(fun object_layer/1, ObjectLayer),
			lists:flatten(ObjectMaps);
		_ ->
			{error, "Unable to parse objects."}
	end.

water_objects(TmxJsonMap) ->
	objects(TmxJsonMap, fun(LayerName) ->
								string:str(LayerName, "water_collision") > 0
			   				end).

base1_objects(TmxJsonMap) ->
	objects(TmxJsonMap, fun(LayerName) ->
							string:str(LayerName, "base1") >= 0 
		   				end).

base2_objects(TmxJsonMap) ->
	objects(TmxJsonMap, fun(LayerName) -> 
							string:str(LayerName, "base2") >= 0
		   				end).

base3_objects(TmxJsonMap) ->
	objects(TmxJsonMap, fun(LayerName) -> 
							string:str(LayerName, "base3") >= 0
		   				end).

base4_objects(TmxJsonMap) ->
	objects(TmxJsonMap, fun(LayerName) -> 
							string:str(LayerName, "base4") >= 0
		   				end).

populate_edges(MapGraph, _, _, []) ->
	MapGraph;
populate_edges(MapGraph, #{height := Height, width := Width} = Dims, Vertex, [Neighbor|Neighbors]) ->
	NeighborRow = maps:get(row, Neighbor),
	NeighborCol = maps:get(col, Neighbor),
	case NeighborRow > 0 andalso NeighborCol > 0 andalso 
			 NeighborRow < Height andalso NeighborCol < Width of
		true ->
			case digraph:vertex(MapGraph, Neighbor) of
				false ->
					NewNeighbor = digraph:add_vertex(MapGraph, Neighbor),
					digraph:add_edge(MapGraph, Vertex, NewNeighbor),
					digraph:add_edge(MapGraph, NewNeighbor, Vertex);
				{V, _} ->
					case bc_map:are_neighbors(#{graph => MapGraph}, V, Vertex) of
						false ->
							digraph:add_edge(MapGraph, Vertex, V),
							digraph:add_edge(MapGraph, V, Vertex);
						true ->
							ok
					end
			end;
		false ->
			ok
	end,
	populate_edges(MapGraph, Dims, Vertex, Neighbors).

populate_vertices(MapGraph, Dims, Vertex) ->
	Row = bc_vertex:row(Vertex),
	Col = bc_vertex:col(Vertex),
	Neighbors = [
				 	Vertex#{row := Row - 1},
					Vertex#{col := Col + 1},
					Vertex#{row := Row + 1},
					Vertex#{col := Col - 1}
			 	],
	case digraph:vertex(MapGraph, Vertex) of
		false ->
			populate_edges(MapGraph, Dims, digraph:add_vertex(MapGraph, Vertex), Neighbors);
		{V, _} ->
			populate_edges(MapGraph, Dims, V, Neighbors)
	end.

do_build_map(MapGraph, Dims, ObjectMapList, Row, Col) ->
	case maps:get(width, Dims) =:= Col of
		false ->
			case inside_object(Dims, ObjectMapList,  Row, Col) of
				false ->
					Vertex = bc_vertex:init(Row, Col),
					populate_vertices(MapGraph, Dims, Vertex);
				true ->
					MapGraph
			end,
			do_build_map(MapGraph, Dims, ObjectMapList, Row, Col + 1);
		true ->
			do_build_map(MapGraph, Dims, ObjectMapList, Row + 1)
	end.
	
do_build_map(MapGraph, Dims, ObjectMapList, Row) ->
	case maps:get(height, Dims) =:= Row of
		false ->
			do_build_map(MapGraph, Dims, ObjectMapList, Row, 0);
		true ->
			MapGraph
	end.
	
do_build_map(MapGraph, Dims, ObjectMapList) ->
	do_build_map(MapGraph, Dims, ObjectMapList, 0).

process_dims(TmxJsonMap) ->
	#{
		height => maps:get(<<"height">>, TmxJsonMap, 0),
		width => maps:get(<<"width">>, TmxJsonMap, 0),
		tileheight => maps:get(<<"tileheight">>, TmxJsonMap, 0),
		tilewidth => maps:get(<<"tilewidth">>, TmxJsonMap, 0)
	}.
  
process_map(TmxJsonMap) ->
	MapGraph = digraph:new([cyclic]),
	Dims = process_dims(TmxJsonMap), 
	ObjectMapList = water_objects(TmxJsonMap),
	do_build_map(MapGraph, Dims, ObjectMapList).

process_objects(TmxJsonMap) ->
	case maps:get(<<"layers">>, TmxJsonMap) of
		LayerList when erlang:is_list(LayerList) ->
			ObjectMapList = lists:filtermap(fun object_layer/1, LayerList),
			lists:flatten(ObjectMapList);
		_ ->
			{error, "Unable to parse objects."}
	end.

process_base_collisions(TmxJsonMap) ->
	MapGraph = process_map(TmxJsonMap),
	Dims = process_dims(TmxJsonMap),
	Base1Objects = base1_objects(TmxJsonMap),
	Base1Objects = base2_objects(TmxJsonMap),
	Base1Objects = base3_objects(TmxJsonMap),
	Base1Objects = base4_objects(TmxJsonMap),
	#{
		base1 => object_verticies(MapGraph, Dims, Base1Objects),
		base2 => object_verticies(MapGraph, Dims, Base1Objects),
		base3 => object_verticies(MapGraph, Dims, Base1Objects),
		base4 => object_verticies(MapGraph, Dims, Base1Objects)
	}.
