
-module(bc_tmx).

%% API exports
-export([load_dims/1,
		 load_base_collision_verticies/1,
		 load_graph/1,
		 load_collision_list/1]).

%% -type dims() :: #{ height :: integer(),
%% 				   width :: integer(),
%% 				   tileheight :: integer(),
%% 				   tilewidth :: width() }.
%% 
%% -type collision() :: #{ id :: integer(),
%% 						name :: string(),
%% 						x :: integer(),
%% 						y :: integer(),
%% 						height :: integer(),
%% 						width :: integer() }.
%% 
%% %% type exports
%% -export_types([dims/0, collision/0]).

%%====================================================================
%% API functions
%%====================================================================

%% @spec load_graph(string()) -> {ok, graph()} || {error, string()}.
load_graph(TmxFile) ->
	{ok, Text} = file:read_file(TmxFile),
	case mochijson:decode(Text) of
		{struct, MapJsonProplist} ->
			{ok, process_map(MapJsonProplist)};
		_ ->
			{error, "Bad map file format."}
	end.

%% @spec load_dims(string()) -> {ok, dims()} || {error, string()}.
load_dims(TmxFile) ->
	{ok, Text} = file:read_file(TmxFile),
	case mochijson:decode(Text) of
		{struct, MapJsonProplist} ->
			{ok, process_dims(MapJsonProplist)};
		_ ->
			{error, "Cannot decode map file."}
	end.

%% @spec load_collision_list(string()) -> {ok, [collision()]} || {error, string()}.
load_collision_list(TmxFile) -> 
	{ok, Text} = file:read_file(TmxFile),
	case mochijson:decode(Text) of
		{struct, MapJsonProplist} ->
			CollisionMapList = process_collisions(MapJsonProplist),
			{ok, lists:flatten(CollisionMapList)};
		_ ->
			{error, "Cannot decode map file."}
	end.

%% @spec load_base_collision_verticies(string()) -> {ok, [collision()]} || {error, string()}.
load_base_collision_verticies(TmxFile) ->
	{ok, Text} = file:read_file(TmxFile),
	case mochijson:decode(Text) of
		{struct, MapJsonProplist} ->
			{ok, process_base_collision_vertices(MapJsonProplist)};
		_ ->
			{error, "Cannot decode map file."}
	end.

%%====================================================================
%% Internal functions
%%====================================================================

object_layer({struct, ObjectLayerProplist}) ->
	case proplists:get_value("objects", ObjectLayerProplist) of
		{array, ObjectList} ->
			{true, lists:map(fun({struct, ObjectProplist}) -> 
									#{
										id => proplists:get_value("id", ObjectProplist, 0),
										name => proplists:get_value("name", ObjectProplist),
										x => proplists:get_value("x", ObjectProplist, 0),
										y => proplists:get_value("y", ObjectProplist, 0),
										height => proplists:get_value("height", ObjectProplist, 0),
										width => proplists:get_value("width", ObjectProplist, 0)
									}
							 end, ObjectList)};
		_ ->
			false
	end;
object_layer(_) ->
	false.

collisions(MapJsonProplist, Filter) ->
	case proplists:get_value("layers", MapJsonProplist) of
		{array, LayerList} ->
			CollisionLayer = lists:filter(fun({struct, LayerProplist}) ->
													   case proplists:get_value("name", LayerProplist) of
														   undefined ->
															   false;
														   LayerName ->
															   Filter(LayerName)
													   end
											   end, LayerList),
			CollisionObjects = lists:filtermap(fun object_layer/1, CollisionLayer),
			lists:flatten(CollisionObjects);
		_ ->
			{error, "Unable to parse water collisions."}
	end.

water_collisions(MapJsonProplist) ->
	collisions(MapJsonProplist, fun(LayerName) -> 
									string:equal(LayerName, "water_collision") 
			   					end).

%% base_collisions(MapJsonProplist) ->
%% 	collisions(MapJsonProplist, fun(LayerName) -> 
%% 										string:str(LayerName, "base") > 0
%% 			   					end).

base1_collisions(MapJsonProplist) ->
	collisions(MapJsonProplist, fun(LayerName) -> 
										string:str(LayerName, "base1") > 0 
			   					end).

base2_collisions(MapJsonProplist) ->
	collisions(MapJsonProplist, fun(LayerName) -> 
										string:str(LayerName, "base2") > 0
			   					end).

base3_collisions(MapJsonProplist) ->
	collisions(MapJsonProplist, fun(LayerName) -> 
										string:str(LayerName, "base3") > 0
			   					end).

base4_collisions(MapJsonProplist) ->
	collisions(MapJsonProplist, fun(LayerName) -> 
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
												map_utils:tile_inside_collision(Dims, RawCollisionMap, Row, Col)
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
					case map_utils:are_neighbors(MapGraph, V, Vertex) of
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
			case map_utils:inside_collision(Dims, CollisionMapList,  Row, Col) of
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

process_dims(MapJsonProplist) ->
	#{
		height => proplists:get_value("height", MapJsonProplist, 0),
		width => proplists:get_value("width", MapJsonProplist, 0),
		tileheight => proplists:get_value("tileheight", MapJsonProplist, 0),
		tilewidth => proplists:get_value("tilewidth", MapJsonProplist, 0)
	}.
  
process_map(MapJsonProplist) ->
	MapGraph = digraph:new([cyclic]),
	Dims = process_dims(MapJsonProplist), 
	CollisionMapList = water_collisions(MapJsonProplist),
	do_build_map(MapGraph, Dims, CollisionMapList).

process_collisions(MapJsonProplist) ->
	case proplists:get_value("layers", MapJsonProplist) of
		{array, LayerList} ->
			CollisionRecords = lists:filtermap(fun object_layer/1, LayerList),
			lists:flatten(CollisionRecords);
		_ ->
			{error, "Unable to parse water collisions."}
	end.

process_base_collision_vertices(MapJsonProplist) ->
	MapGraph = process_map(MapJsonProplist),
	Dims = process_dims(MapJsonProplist),
	Base1Collisions = base1_collisions(MapJsonProplist),
	Base2Collisions = base2_collisions(MapJsonProplist),
	Base3Collisions = base3_collisions(MapJsonProplist),
	Base4Collisions = base4_collisions(MapJsonProplist),
	#{
		base1 => collision_verticies(MapGraph, Dims, Base1Collisions),
		base2 => collision_verticies(MapGraph, Dims, Base2Collisions),
		base3 => collision_verticies(MapGraph, Dims, Base3Collisions),
		base4 => collision_verticies(MapGraph, Dims, Base4Collisions)
	}.
