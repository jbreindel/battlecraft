
-module(bc_game).

-export([create/3,
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

-spec create(Id :: integer(), 
			 Event :: pid(), 
			 Fsm :: pid()) -> game().
create(Id, Event, Fsm) ->
	#{id => Id, event => Event, fsm => Fsm}.

-spec id(BcGame :: game()) -> integer().
id(BcGame) ->
	maps:get(id, BcGame).

-spec event(BcGame :: game()) -> pid().
event(BcGame) ->
	maps:get(event, BcGame).

-spec fsm(BcGame :: game()) -> pid().
fsm(BcGame) ->
	maps:get(fsm, BcGame).
