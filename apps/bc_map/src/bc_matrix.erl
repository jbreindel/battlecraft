
-module(bc_matrix).

-export([init/1,
		 max_row/1,
		 min_row/1,
		 row/2,
		 max_col/1,
		 min_col/1,
		 col/2,
		 dimensions/1]).

%% ====================================================================
%% API functions
%% ====================================================================

-spec init(BcVertices :: [bc_vertex:vertex()]) -> dict:dict(Row :: integer(), 
													   		ColSet :: sets:set(Col :: integer())).
init(BcVertices) ->
	lists:foldl(fun(BcVertex, BcMatrix) -> 
					Row = bc_vertex:row(BcVertex),
					Col = bc_vertex:col(BcVertex),
					case dict:find(Row, BcMatrix) of
						{ok, ColSet} ->
							UpdatedColSet = sets:add_element(Col, ColSet),
							dict:store(Row, UpdatedColSet, BcMatrix);
						error ->
							NewColSet = sets:add_element(Col, sets:new()),
							dict:store(Row, NewColSet, BcMatrix)
					end
				end, dict:new(), BcVertices).

-spec max_row(BcMatrix :: dict:dict()) -> integer().
max_row(BcMatrix) ->
	Keys = dict:fetch_keys(BcMatrix),
	lists:max(Keys).

-spec min_row(BcMatrix :: dict:dict()) -> integer().
min_row(BcMatrix) ->
	Keys = dict:fetch_keys(BcMatrix),
	lists:min(Keys).

-spec row(Row :: integer(), BcMatrix :: dict:dict()) -> {ok, [bc_vertex:vertex()]} | error.
row(Row, BcMatrix) ->
	case dict:find(Row, BcMatrix) of
		{ok, ColSet} ->
			{ok, lists:map(fun(Col) -> 
						       bc_vertex:init(Row, Col) 
						   end, sets:to_list(ColSet))};
		error ->
			error
	end.

-spec max_col(BcMatrix :: dict:dict()) -> integer().
max_col(BcMatrix) ->
	ColValues = col_values(BcMatrix),
	lists:max(ColValues).

-spec min_col(BcMatrix :: dict:dict()) -> integer().
min_col(BcMatrix) ->
	ColValues = col_values(BcMatrix),
	lists:min(ColValues).

-spec col(Col :: integer(), BcMatrix :: dict:dict()) -> {ok, [bc_vertex:vertex()]} | error.
col(Col, BcMatrix) ->
	case dict:fold(fun(Row, ColSet, BcVertices) -> 
			      		case sets:is_element(Col, ColSet) of
					  		true ->
						  		BcVertices ++ [bc_vertex:init(Row, Col)];
					  		false ->
						  		BcVertices
				  		end
			  		end, [], BcMatrix) of
		Cols when length(Cols) > 0 ->
			{ok, Cols};
		Cols when length(Cols) == 0 ->
			error
	end.

-spec dimensions(BcMatrix :: dict:dict()) -> {integer(), integer()} | 
												 error.
dimensions(BcMatrix) ->
	case dict:fetch_keys(BcMatrix) of
		[] ->
			{0, 0};
		[Row|_] = Rows ->
			case dict:find(Row, BcMatrix) of
				{ok, Cols} ->
					{length(Rows), length(Cols)};
				error ->
					error
			end
	end.
				

%% ====================================================================
%% Internal functions
%% ====================================================================

col_values(BcMatrix) ->
	ValueSet = dict:fold(fun(_, ColSet, Values) -> 
						 	sets:union(ColSet, Values) 
						 end, sets:new(), BcMatrix),
	sets:to_list(ValueSet).
