
-module(bc_player_serv).
-behavior(gen_server).

%% api functions
-export([start_link/6]).

%% gen_server callbacks
-export([init/1]).

%% state rec
-record(state, {player_sup,
				game,
				player,
				player_num,
				gold,
				map,
				entities}).

%%====================================================================
%% API functions
%%====================================================================

start_link(BcPlayerSup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities) ->
	gen_server:start_link(?MODULE, [BcPlayerSup, 
									BcGame, 
									BcPlayer, 
									BcGoldFsm, 
									BcMap, 
									BcEntities], []).

%%====================================================================
%% Gen_server functions
%%====================================================================

init([BcPlayerSup, BcGame, BcPlayer, BcGoldFsm, BcMap, BcEntities]) ->
	{ok, #state{player_sup = BcPlayerSup,
				game = BcGame, 
				player = BcPlayer,
				gold = BcGoldFsm,
				map = BcMap,
				entities = BcEntities}}.

%%====================================================================
%% Internal functions
%%====================================================================

base_num(BaseBcVertices, BcMap) ->
	BaseBcVertex = lists:nth(1, BaseBcVertices),
	case lists:any(fun(Base1BcVertex) -> 
					  Base1BcVertex =:= BaseBcVertex 
				   end, bc_map:base1_vertices(BcMap)) of
		true -> 1;
		false -> 
			case lists:any(fun(Base2BcVertex) -> 
					  Base2BcVertex =:= BaseBcVertex 
				   end, bc_map:base2_vertices(BcMap)) of
				true -> 2;
				false ->
					case lists:any(fun(Base3BcVertex) -> 
									  Base3BcVertex =:= BaseBcVertex 
								   end, bc_map:base3_vertices(BcMap)) of
						true -> 3;
						false ->
							case lists:any(fun(Base4BcVertex) -> 
											  Base4BcVertex =:= BaseBcVertex 
										   end, bc_map:base4_vertices(BcMap)) of
								true -> 4;
								false -> undefined
							end
					end
			end
	end.

player_num(#state{player = BcPlayer,
				  player_num = PlayerNum,
				  map = BcMap, 
				  entities = BcEntities} = State) ->
	PlayerId = bc_player:id(BcPlayer),
	BaseBcEntities = bc_entity:query_type(base, BcEntities),
	case lists:filter(fun(BaseBcEntity) -> 
						  PlayerId =:= bc_entity:player_id(BaseBcEntity) 
					  end, BaseBcEntities) of
		[] ->
			undefined;
		[PlayerBaseBcEntity] ->
			Uuid = bc_entity:uuid(PlayerBaseBcEntity),
			QueryResults = bc_map:query_collisions(Uuid, BcEntities),
			BcVertices = lists:map(fun(#{vertex := BcVertex}) -> BcVertex end, QueryResults),
			base_num(BcVertices)
	end.

