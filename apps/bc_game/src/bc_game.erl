
-module(bc_game).
-include("bc_game.hrl").

-export([init_model/0,
		 create/3,
		 id/1, 
		 event/1, 
		 fsm/1]).

%%
%% @doc game map for game related info
%%
-type game() :: #{id => integer(),
				  event => pid(),
				  fsm => pid()}.

-export_types([game/0]).

init_model() ->
	bc_model:init(),
	Tables = [#{name => player,
				attributes => record_info(fields, player)},
			  #{name => game,
				attributes => record_info(fields, game)},
			  #{name => gp_assoc,
				attributes => record_info(fields, gp_assoc)}],
	bc_model:create_tables([node()], Tables).

create(Id, Event, Fsm) ->
	#{id => Id, event => Event, fsm => Fsm}.

id(BcGame) ->
	maps:get(id, BcGame).

event(BcGame) ->
	maps:get(event, BcGame).

fsm(BcGame) ->
	maps:get(fsm, BcGame).
