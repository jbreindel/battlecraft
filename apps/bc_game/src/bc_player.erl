
-module(bc_player).

-export([create/3, 
		 id/1, 
		 handle/1, 
		 pid/1]).

%%
%% @doc player map for player indentification
%%
-type player() :: #{id => integer(),
					handle => string(),
					team => integer(),
					pid => pid()}.

-spec create(Id :: integer(), 
			 Handle :: string(),
			 Team :: integer(),
			 PlayerPid :: pid()) -> player().
create(Id, Handle, Team, PlayerPid) ->
	#{id => Id, handle => Handle, team => Team, pid => PlayerPid}.

-spec id(BcPlayer :: player()) -> integer().
id(BcPlayer) ->
	maps:get(id, BcPlayer).

-spec handle(BcPlayer :: player()) -> string().
handle(BcPlayer) ->
	maps:get(handle, BcPlayer).

-spec team(BcPlayer :: player()) -> integer().
team(BcPlayer) ->
	maps:get(team, BcPlayer).

-spec pid(BcPlayer :: player()) -> pid().
pid(BcPlayer) ->
	maps:get(pid, BcPlayer).