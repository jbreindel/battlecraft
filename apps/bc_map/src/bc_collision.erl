
-module(bc_collision).

%% api exports
-export([init/2, 
		 uuid/1, 
		 vertices/1,
		 set_vertices/2,
		 move/2,
		 difference_vertices/2]).

%%
% @doc collision map
%%
-type collision() :: #{uuid => uuid:uuid(), 
					   vertices => [bc_vertex:vertex()]}.

%% type exports 
-export_type([collision/0]).

%%====================================================================
%% API functions
%%====================================================================

-spec init(Uuid :: uuid:uuid(),
		   BcVertices :: bc_vertex:vertex() | [bc_vertex:vertex()]) -> collision().
init(Uuid, BcVertices) when is_list(BcVertices) ->
	#{uuid => Uuid, vertices => BcVertices};
init(Uuid, BcVertex) ->
	init(Uuid, [BcVertex]).

-spec uuid(BcCollision :: collision()) -> uuid:uuid().
uuid(BcCollision) ->
	maps:get(uuid, BcCollision).

-spec vertices(BcCollision :: collision()) -> [bc_vertex:vertex()].
vertices(BcCollision) ->
	maps:get(vertices, BcCollision).

-spec set_vertices(BcVertices :: [bc_vertex:vertex()], 
				   BcCollision :: collision()) -> collision().
set_vertices(BcVertices, BcCollision) ->
	maps:update(vertices, BcVertices, BcCollision).

-spec move(Direction :: atom(), 
		   BcCollision :: collision()) -> collision().
move(Direction, BcCollision) ->
	UpdatedVertices = lists:map(fun(BcVertex) -> 
									bc_vertex:move(Direction, BcVertex) 
								end, vertices(BcCollision)),
	set_vertices(UpdatedVertices, BcCollision).

-spec difference_vertices(BcCollision1 :: collision(), 
						  BcCollision :: collision()) -> [bc_vertex:vertex()].
difference_vertices(BcCollision1, BcCollision2) ->
	Vertices1 = vertices(BcCollision1),
	Vertices2 = vertices(BcCollision2),
	Set1 = sets:from_list(Vertices1),
	Set2 = sets:from_list(Vertices2),
	DiffSet = sets:subtract(Set1, Set2),
	sets:to_list(DiffSet).