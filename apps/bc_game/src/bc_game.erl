
-module(bc_game).

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

%% game type
-export_types([game/0]).

create(Id, Event, Fsm) ->
	#{id => Id, event => Event, fsm => Fsm}.

id(BcGame) ->
	maps:get(id, BcGame).

event(BcGame) ->
	maps:get(event, BcGame).

fsm(BcGame) ->
	maps:get(fsm, BcGame).
