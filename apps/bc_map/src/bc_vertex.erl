
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

compare_rows(Int1, Int2) when Int1 < Int2 ->
	lt;
compare_rows(Int1, Int2) when Int1 > Int2 ->
	gt;
compare(Int1, Int2) when Int1 == Int2 ->
	equal.

-spec compare(BcVertex1 :: vertex(), 
			  BcVertex2 :: vertex()) -> boolean().
compare(BcVertex1, BcVertex2) ->
	Row1 = row(BcVertex1),
	case row(BcVertex2) of
		Row2 when Row1 < Row2 ->
			true;
		Row2 when Row1 > Row2 ->
			false;
		Row2 when Row1 == Row2 ->
			Col1 = col(BcVertex1),
			case col(BcVertex2) of
				Col2 when Col1 < Col2 ->
					true;
				Col2 when Col1 > Col2 ->
					false;
				_ ->
					true
			end
	end.
