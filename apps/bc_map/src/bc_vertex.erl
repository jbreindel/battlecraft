
-module(bc_vertex).

-export([init/2, 
		 row/1, 
		 col/1,
		 to_tuple/1,
		 compare/2]).
		 
%%
% @doc vertex structure
%%
-type vertex() :: #{row => integer(), col => integer()}.

%% type exports
-export_type([vertex/0]).

-spec init(Row :: integer(), 
		   Col :: integer()) -> vertex().
init(Row, Col) ->
	#{row => Row, col => Col}.
	
-spec row(BcVertex :: vertex()) -> integer().
row(BcVertex) ->
	maps:get(row, BcVertex).
	
-spec col(BcVertex :: vertex()) -> integer().
col(BcVertex) ->
	maps:get(col, BcVertex).

-spec to_tuple(BcVertex :: vertex()) -> tuple().
to_tuple(#{row := Row, col := Col}) ->
	{Row, Col}.

compare_rc(Int1, Int2) when Int1 < Int2 ->
	lt;
compare_rc(Int1, Int2) when Int1 > Int2 ->
	gt;
compare_rc(Int1, Int2) when Int1 == Int2 ->
	equal.

-spec compare(BcVertex1 :: vertex(), 
			  BcVertex2 :: vertex()) -> boolean().
compare(BcVertex1, BcVertex2) ->
	Row1 = row(BcVertex1),
	Row2 = row(BcVertex2),
	case compare_rc(Row1, Row2) of
		lt -> true;
		gt -> false;
		equal ->
			Col1 = col(BcVertex1),
			Col2 = col(BcVertex2),
			case compare_rc(Col1, Col2) of
				lt -> true;
				gt -> false;
				equal -> true
			end
	end.

-spec intersect(BcVertices1 :: [vertex()], 
				BcVertices2 :: [vertex()]) ->  [vertex()].
intersect(BcVertices1, BcVertices2) ->
	Set1 = sets:from_list(BcVertices1),
	Set2 = sets:from_list(BcVertices2),
	Intersection = sets:intersection(Set1, Set2),
	sets:to_list(Intersection).
