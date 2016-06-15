%%%-------------------------------------------------------------------
%% @doc bc_game public API
%% @end
%%%-------------------------------------------------------------------

-module(bc_game_app).
-behavior(application).
-include("bc_game.hrl").

%% Application callbacks
-export([start/2,
		 stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
	init_model(),
    bc_manager_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

init_model() ->
	bc_model:init(),
	Tables = [#{name => player,
				attributes => record_info(fields, player)},
			  #{name => game,
				attributes => record_info(fields, game)},
			  #{name => gp_assoc,
				attributes => record_info(fields, gp_assoc)}],
	bc_model:create_tables([node()], Tables).
