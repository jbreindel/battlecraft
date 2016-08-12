
-module(bc_vertex).

-export([init/2, 
		 row/1, 
		 col/1,
		 to_tuple/1]).
		 
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
