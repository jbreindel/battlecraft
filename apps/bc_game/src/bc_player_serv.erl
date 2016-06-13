
-module(bc_player_serv).
-behavior(gen_server).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcEntitySup, BcMoneyFsm, MapGraph, CollisionTab) ->
