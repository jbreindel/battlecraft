
-module(bc_vertex).

-export([create/2, 
		 row/1, 
		 col/1]).
		 
%%
% @doc vertex structure
%%
-type vertex() :: #{row => integer(), col => integer()}.

%% type exports
-export_type([vertex()]).

-spec create(Row :: integer(), 
			 Col :: integer()) -> vertex().
create(Row, Col) ->
	#{row => Row, col => Col}.
	
-spec row(BcVertex :: vertex()) -> integer().
row(BcVertex) ->
	maps:get(row, BcVertex).
	
-spec col(BcVertex :: vertex()) -> integer().
col(BcVertex) ->
	maps:get(col, BcVertex).
