%%%%
%%%% Record Definitions
%%%%

%% Player - user who plays
-record(player, {
			%% id key
			id,
			%% username handle
			handle,
			%% team that player is on
			team,
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
			%% max number of players
			max_players,
			%% timestamp for created
			created,
			%% timestamp for last modified
			modified
	}).

%% gp_assoc - association between games and players
-record(gp_assoc, {
			%% id for the rec
			id,
			%% id for game
			game_id,
			%% id for player
			player_id
	}).

