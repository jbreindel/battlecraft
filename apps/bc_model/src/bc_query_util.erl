
-module(bc_query_util).
-include_lib("stdlib/include/qlc.hrl").

-export([mnesia_query/3]).

mnesia_query(Gen, Offset, Limit) ->
	mnesia:transaction(fun() -> 
					   	  Qh = Gen(),
						  if Offset > 0 ->
								qlc:next_answers(Qh, Offset);
							 true ->
								 ok
						  end,
						  qlc:next_answers(Qh, Limit)
					   end).


