
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
	{ok, process_base_collision_vertices(TmxJsonMap)}.

%%====================================================================
%% Internal functions
%%====================================================================

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
populate_edges(MapGraph, Dims, Vertex, [Neighbor|Neighbors]) ->
	NeighborRow = maps:get(row, Neighbor),
	NeighborCol = maps:get(col, Neighbor),
	case NeighborRow > 0 andalso NeighborCol > 0 of
		false ->
			ok;
		true ->
			case digraph:vertex(MapGraph, Neighbor) of
				false ->
					NewNeighbor = digraph:add_vertex(MapGraph, Neighbor),
					digraph:add_edge(MapGraph, Vertex, NewNeighbor),
					digraph:add_edge(MapGraph, NewNeighbor, Vertex);
				{V, _} ->
					case bc_map_utils:are_neighbors(MapGraph, V, Vertex) of
						false ->
							digraph:add_edge(MapGraph, Vertex, V),
							digraph:add_edge(MapGraph, V, Vertex);
						true ->
							ok
					end
			end
	end,
	populate_edges(MapGraph, Dims, Vertex, Neighbors).

populate_vertices(MapGraph, Dims, ObjectMapList, Vertex) ->
	Row = maps:get(row, Vertex),
	Col = maps:get(col, Vertex),
	Neighbors = [
				 	Vertex#{row := Row - 1},
					Vertex#{col := Col + 1},
					Vertex#{row := Row + 1},
					Vertex#{row := Col - 1}
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
			case bc_map_utils:inside_object(Dims, ObjectMapList,  Row, Col) of
				false ->
					Vertex = #{row => Row, col => Col},
					populate_vertices(MapGraph, Dims, ObjectMapList, Vertex);
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
			{error, "Unable to parse water collisions."}
	end.

process_base_collisions(TmxJsonMap) ->
	MapGraph = process_map(TmxJsonMap),
	Dims = process_dims(TmxJsonMap),
	Base1Collisions = base1_collisions(TmxJsonMap),
	Base2Collisions = base2_collisions(TmxJsonMap),
	Base3Collisions = base3_collisions(TmxJsonMap),
	Base4Collisions = base4_collisions(TmxJsonMap),
	#{
		base1 => collision_verticies(MapGraph, Dims, Base1Collisions),
		base2 => collision_verticies(MapGraph, Dims, Base2Collisions),
		base3 => collision_verticies(MapGraph, Dims, Base3Collisions),
		base4 => collision_verticies(MapGraph, Dims, Base4Collisions)
	}.
