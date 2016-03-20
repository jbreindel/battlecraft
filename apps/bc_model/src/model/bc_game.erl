
-module(bc_game, [Id, 
			State::integer(),
			WinnerId::string(),
			IsPrivate::boolean(),
			Created::timestamp(),
			Modified::timestamp()]).

-has({bc_players, many}).

-compile(export_all).

before_create() ->
	set(created, os:timestamp()),
	set(modified, os:timestamp()),
	ok.

before_update() ->
	set(modified, os:timestamp()),
	ok.
