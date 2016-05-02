
-module(bc_game).
-include("bc_game.hrl").

init_model() ->
	Tables = [#{name => player,
				attributes => record_info(player)},
			  #{name => game,
				attributes => record_info(game)},
			  #{name => gp_assoc,
				attributes => record_info(gp_assoc)}],
	bc_model:create_tables([node()], Tables).
