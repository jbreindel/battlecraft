
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
					pid => pid()}.

-spec create(Id :: integer(), 
			 Handle :: string(), 
			 PlayerPid :: pid()) -> player().
create(Id, Handle, PlayerPid) ->
	#{id => Id, handle => Handle, pid => PlayerPid}.

-spec id(BcPlayer :: player()) -> integer().
id(BcPlayer) ->
	maps:get(id, BcPlayer).

-spec handle(BcPlayer :: player()) -> string().
handle(BcPlayer) ->
	maps:get(handle, BcPlayer).

-spec pid(BcPlayer :: player()) -> pid().
pid(BcPlayer) ->
	maps:get(pid, BcPlayer).