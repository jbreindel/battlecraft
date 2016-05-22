
-module(bc_game).
-include("bc_game.hrl").

-export([init_model/0]).

init_model() ->
	bc_model:init(),
	Tables = [#{name => player,
				attributes => record_info(fields, player)},
			  #{name => game,
				attributes => record_info(fields, game)},
			  #{name => gp_assoc,
				attributes => record_info(fields, gp_assoc)}],
	bc_model:create_tables([node()], Tables).
