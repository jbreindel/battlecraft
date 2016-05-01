%%%%
%%%% Record Definitions
%%%%



%% Player - user who plays
-record(player, {
			%% id key
			id,
			%% username handle
			handle,
			%% out status of the game
			is_out,
			%% timestamp for created
			created,
			%% timestamp for last modified
			modified
}).

%% Game - a game record with multiple players
-record(game, { 
			%% id key
			id,
			%% game state integer
			state,
			%% winner id field
			winner_id,
			%% privacy field
			is_private,
			%% timestamp for created
			created,
			%% timestamp for last modified
			modified
	}).

%% gp_assoc - association between games and players
-record(gp_assoc, {
			%% id for game
			game_id,
			%% id for player
			player_id				   
}).

