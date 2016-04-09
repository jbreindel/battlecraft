
-module(bc_game_serv).
-behavior(gen_server).

%% api functions
-export([start_link/0,
		 create_game/0,
		 join_game/1
		]).

%% gen_server callbacks
-export(init/1,
		handle_call/3,
		handle_cast/2).

