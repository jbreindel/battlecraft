
-module(bc_game).

-export([init/3,
		 id/1,
		 privacy/1,
		 max_players/1,
		 event/1, 
		 fsm/1]).

%%
%% @doc game map for game related info
%%
-type game() :: #{id => integer(),
				  privacy => integer(),
				  max_players => integer(),
				  event => pid(),
				  fsm => pid()}.

%% game type
-export_types([game/0]).

-spec init(Id :: integer(),
		   Privacy :: integer(),
		   MaxPlayers :: integer(),
		   Event :: pid(), 
		   Fsm :: pid()) -> game().
init(Id, Privacy, MaxPlayers, Event, Fsm) ->
	#{id => Id, 
	  privacy => Privacy, 
	  max_players => MaxPlayers, 
	  event => Event, 
	  fsm => Fsm}.

-spec id(BcGame :: game()) -> integer().
id(BcGame) ->
	maps:get(id, BcGame).

-spec privacy(BcGame :: game()) -> integer().
privacy(BcGame) ->
	maps:get(privacy, BcGame).

-spec max_players(BcGame :: game()) -> integer().
max_players(BcGame) ->
	maps:get(max_players, BcGame).

-spec event(BcGame :: game()) -> pid().
event(BcGame) ->
	maps:get(event, BcGame).

-spec fsm(BcGame :: game()) -> pid().
fsm(BcGame) ->
	maps:get(fsm, BcGame).
