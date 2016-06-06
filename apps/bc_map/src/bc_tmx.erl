
-module(bc_tmx).

%% API exports
-export([load_dims/1,
		 load_base_collision_verticies/1,
		 load_graph/1,
		 load_collision_list/1
		]).

%% type exports
-export_types([dims/0, collision/0]).

-type dims() :: #{height => integer(),
				  width => integer(),
				  tileheight => integer(),
				  tilewidth => integer()}.

-type collision() :: #{id => integer(),
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

-spec load_collision_list(map()) -> {ok, [collision()]} | {error, string()}.
load_collision_list(TmxJsonMap) ->
	CollisionMapList = process_collisions(TmxJsonMap),
	{ok, lists:flatten(CollisionMapList)}.

-spec load_base_collision_verticies(map()) -> {ok, [collision()]} | {error, string()}.
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

collisions(TmxJsonMap, Filter) ->
	case maps:get(<<"layers">>, TmxJsonMap) of
		LayerList when erlang:is_list(LayerList) ->
			CollisionLayer = lists:filter(fun(LayerMap) ->
											   case maps:get(<<"name">>, LayerMap, undefined) of
												   undefined ->
													   false;
												   BinLayerName ->
													   LayerName = binary:bin_to_list(BinLayerName),
													   Filter(LayerName)
											   end
										   end, LayerList),
			CollisionObjectMaps = lists:filtermap(fun object_layer/1, CollisionLayer),
			lists:flatten(CollisionObjectMaps);
		_ ->
			{error, "Unable to parse water collisions."}
	end.

water_collisions(TmxJsonMap) ->
	collisions(TmxJsonMap, fun(LayerName) ->
								string:str(LayerName, "water_collision") > 0
			   				end).

%% base_collisions(MapJsonProplist) ->
%% 	collisions(MapJsonProplist, fun(LayerName) -> 
%% 										string:str(LayerName, "base") > 0
%% 			   					end).

base1_collisions(TmxJsonMap) ->
	collisions(TmxJsonMap, fun(LayerName) ->
								string:str(LayerName, "base1") >= 0 
			   				end).

base2_collisions(TmxJsonMap) ->
	collisions(TmxJsonMap, fun(LayerName) -> 
								string:str(LayerName, "base2") >= 0
			   				end).

base3_collisions(TmxJsonMap) ->
	collisions(TmxJsonMap, fun(LayerName) -> 
								string:str(LayerName, "base3") >= 0
			   				end).

base4_collisions(TmxJsonMap) ->
	collisions(TmxJsonMap, fun(LayerName) -> 
								string:str(LayerName, "base4") >= 0
			   				end).

collision_verticies(MapGraph, Dims, RawCollisionMaps) ->
	collision_verticies(MapGraph, Dims, RawCollisionMaps, []).

collision_verticies(_, _, [], Verticies) ->
	Verticies;
collision_verticies(MapGraph, Dims, [RawCollisionMap|RawCollisionMaps], Verticies) ->
	VerticiesAcc = Verticies ++ lists:filter(fun(Vertex) -> 
												Row = maps:get(row, Vertex),
												Col = maps:get(col, Vertex),
												bc_map_utils:tile_inside_collision(Dims, RawCollisionMap, Row, Col)
											end, digraph:vertices(MapGraph)),
	collision_verticies(MapGraph, Dims, RawCollisionMaps, VerticiesAcc).

populate_edges(MapGraph, _, _, _, []) ->
	MapGraph;
populate_edges(MapGraph, Dims, CollisionMapList, Vertex, [Neighbor|Neighbors]) ->
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
	populate_edges(MapGraph, Dims, CollisionMapList, Vertex, Neighbors).

populate_vertices(MapGraph, Dims, CollisionMapList, Vertex) ->
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
			populate_edges(MapGraph, Dims, CollisionMapList, digraph:add_vertex(MapGraph, Vertex), Neighbors);
		{V, _} ->
			populate_edges(MapGraph, Dims, CollisionMapList, V, Neighbors)
	end.

do_build_map(MapGraph, Dims, CollisionMapList, Row, Col) ->
	case maps:get(width, Dims) =:= Col of
		false ->
			case bc_map_utils:inside_collision(Dims, CollisionMapList,  Row, Col) of
				false ->
					Vertex = #{row => Row, col => Col},
					populate_vertices(MapGraph, Dims, CollisionMapList, Vertex);
				true ->
					MapGraph
			end,
			do_build_map(MapGraph, Dims, CollisionMapList, Row, Col + 1);
		true ->
			do_build_map(MapGraph, Dims, CollisionMapList, Row + 1)
	end.
	
do_build_map(MapGraph, Dims, CollisionMapList, Row) ->
	case maps:get(height, Dims) =:= Row of
		false ->
			do_build_map(MapGraph, Dims, CollisionMapList, Row, 0);
		true ->
			MapGraph
	end.
	
do_build_map(MapGraph, Dims, CollisionMapList) ->
	do_build_map(MapGraph, Dims, CollisionMapList, 0).

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
	CollisionMapList = water_collisions(TmxJsonMap),
	do_build_map(MapGraph, Dims, CollisionMapList).

process_collisions(TmxJsonMap) ->
	case maps:get(<<"layers">>, TmxJsonMap) of
		LayerList when erlang:is_list(LayerList) ->
			CollisionMapList = lists:filtermap(fun object_layer/1, LayerList),
			lists:flatten(CollisionMapList);
		_ ->
			{error, "Unable to parse water collisions."}
	end.

process_base_collision_vertices(TmxJsonMap) ->
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
