
-module(bc_collision).

%% ets table options
-define(ETS_OPTIONS(Heir), [set, 
							public, 
							{heir, Heir, []}, 
							{write_concurrency, true}]).

%% api functions
-export([init/1, 
		 insert/2, 
		 query/2, update/3, 
		 delete/2]).

%% type exports
-export_type([collision/0, query_res/0]).

%%
%% @doc collision map
%%
-type collision() :: #{id => string(), vertices => [bc_map:vertex()]}.

%%
%% @doc query result rows
%%
-type query_res() :: #{id => string(), vertex => bc_map:vertex()}.

%%====================================================================
%% Api functions
%%====================================================================

-spec init(BcGameSup :: pid()) -> ets:tid().
init(BcGameSup) ->
	ets:new(collision, ?ETS_OPTIONS(BcGameSup)).

-spec insert(Tab :: ets:tid(),
				 CollisionMap :: bc_collision:collision()) -> boolean().
insert(Tab, CollisionMap) ->
	Rows = vertex_rows(CollisionMap),
	ets:insert_new(Tab, Rows).

-spec query(Tab :: ets:tid(),
			Vertices :: [bc_map:vertex()]) -> [query_res()].
query(Tab, Vertices) ->
	Ms = collision_ms(Vertices),
	case ets:select(Tab, Ms) of
		Results when is_list(Results) andalso length(Results) > 0 ->
			lists:map(fun({{Row, Col}, Id}) -> #{id => Id, 
												 vertex => #{row => Row, 
															 col => Col}} end, Results);
		_ ->
			{error, "Unable to query collisions."}
	end.

-spec update(Tab :: ets:tid(), 
			 OriginalCollisionMap :: bc_collision:collision(), 
			 UpdatedCollisionMap :: bc_collision:collision()) -> ok | {error, Reason :: string()}.
update(Tab, OriginalCollisionMap, UpdatedCollisionMap) ->
	case difference_vertices(UpdatedCollisionMap, OriginalCollisionMap) of
		InsertRows when length(InsertRows) > 0 ->
			case ets:insert_new(Tab, InsertRows) of
				true ->
					DifferenceRows = difference_vertices(OriginalCollisionMap, UpdatedCollisionMap),
					Ms = collision_ms(DifferenceRows),
					ets:match_delete(Tab, Ms),
					ok;
				false ->
					{error, "Unable to insert new vertices."}
			end;
		_ ->
			{error, "Updated collision doesn't have different vertices."}
	end.

-spec delete(Tab :: ets:tid(), 
			 CollisionMap :: bc_collision:collision()) -> true.
delete(Tab, CollisionMap) ->
	Ms = collision_ms(CollisionMap),
	ets:match_delete(Tab, Ms).

%%====================================================================
%% Internal functions
%%====================================================================

vertex_rows(#{id := Id, vertices := Vertices} = CollisionMap) when erlang:is_map(CollisionMap) ->
	vertex_rows(Id, Vertices).

vertex_rows(Id, Vertices) when erlang:is_list(Vertices) ->
	lists:map(fun(#{row := Row, col := Col}) -> {{Row, Col}, Id} end, Vertices).

difference_vertices(#{vertices := Vertices1} = CollisionMap1,
				  #{vertices := Vertices2} = CollisionMap2) ->
	Set1 = sets:from_list(Vertices1),
	Set2 = sets:from_list(Vertices2),
	DiffSet = sofs:difference(Set1, Set2),
	sets:to_list(DiffSet).

collision_ms(#{vertices := Vertices} = CollisionMap) when is_map(CollisionMap) ->
	collision_ms(Vertices);
collision_ms(Vertices) when is_list(Vertices) ->
	[{{{'$1','$2'},'_'},
	  [{'andalso',{'=:=','$1',Row},
		{'=:=','$2',Col}}],['$_']} || #{row := Row, col := Col} <- Vertices].
