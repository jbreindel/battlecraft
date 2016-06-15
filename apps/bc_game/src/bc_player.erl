
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

create(Id, Handle, PlayerPid) ->
	#{id => Id, handle => Handle, pid => PlayerPid}.

id(BcPlayer) ->
	maps:get(id, BcPlayer).

handle(BcPlayer) ->
	maps:get(handle, BcPlayer).

pid(BcPlayer) ->
	maps:get(pid, BcPlayer).