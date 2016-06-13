
-module(bc_input_serv).
-behavior(gen_server).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcEntitySup, MapGraph, CollisionTab, BcGoldFsm) ->
	gen_server:start_link(?MODULE, BcEntitySup, []).
