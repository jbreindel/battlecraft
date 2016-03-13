
-module(bc_player, [Id, 
					Handle::string(),
					IsOut::boolean(),
					Created::timestamp(),
					Modified::timestamp(),
					BcGameId]).

-belongs_to(bc_game).

-compile(export_all).

before_create() ->
	set(created, os:timestamp()),
	ok.

before_update() ->
	set(modified, os:timestamp()),
	ok.