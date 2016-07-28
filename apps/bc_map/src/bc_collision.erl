
-module(bc_collision).

%% api exports
-export([init/2, 
		 uuid/1, 
		 vertices/1,
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
		   Vertices :: [bc_vertex:vertex()]) -> collision().
init(Uuid, Vertices) ->
	#{uuid => Uuid, vertices => Vertices}.

-spec uuid(BcCollision :: collision()) -> uuid:uuid().
uuid(BcCollision) ->
	maps:get(uuid, BcCollision).

-spec vertices(BcCollision :: collision()) -> [bc_vertex:vertex()].
vertices(BcCollision) ->
	maps:get(vertices, BcCollision).

difference_vertices(#{vertices := Vertices1} = BcCollision1,
				    #{vertices := Vertices2} = BcCollision2) ->
	Set1 = sets:from_list(Vertices1),
	Set2 = sets:from_list(Vertices2),
	DiffSet = sofs:difference(Set1, Set2),
	sets:to_list(DiffSet).