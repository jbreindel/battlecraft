
-module(bc_game).
-include("bc_game.hrl").

init_model() ->
	Tables = [#{name => player,
				attributes => record_info(fields, player)},
			  #{name => game,
				attributes => record_info(fields, game)},
			  #{name => gp_assoc,
				attributes => record_info(fields, gp_assoc)}],
	bc_model:create_tables([node()], Tables).
