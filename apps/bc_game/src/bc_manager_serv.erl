
-module(bc_manager_serv).
-behavior(gen_server).
-include("../include/bc_game_state.hrl").

%% api functions
-export([start_link/0,
		 create_game/2,
		 get_game/2,
		 remove_game/2]).

%% gen_server callbacks
-export([init/1,
		 handle_call/3,
		 handle_cast/2, 
		 handle_info/2, 
		 terminate/2, 
		 code_change/3]).

%% state rec
-record(state, {manager_sup,
				games}).

%%====================================================================
%% API functions
%%====================================================================

-spec start_link() -> gen:start_ret().
start_link() ->
	gen_server:start_link({local, bc_manager_serv}, ?MODULE, [], []).

-spec create_game(BcManagerServ :: pid(), 
				  Privacy :: integer()) -> {ok, GameId :: integer(), BcGameFsm :: pid()} | 
											   {error, Reason :: string()}.
create_game(BcManagerServ, Privacy) ->
	gen_server:call(BcManagerServ, {create_game, Privacy}).

-spec get_game(BcManagerServ :: pid(), 
			   GameId :: integer()) -> {ok, BcGameFsm :: pid()} | 
										   {error, not_found}.
get_game(BcManagerServ, GameId) ->
	gen_server:call(BcManagerServ, {get_game, GameId}).

-spec remove_game(BcManagerServ :: pid(), 
				  GameId :: integer()) -> ok.
remove_game(BcManagerServ, GameId) ->
	gen_server:cast(BcManagerServ, {remove_game, GameId}).

%%====================================================================
%% Gen_server callbacks
%%====================================================================

-spec init(Args :: term()) -> Result when
	Result :: {ok, State}
			| {ok, State, Timeout}
			| {ok, State, hibernate}
			| {stop, Reason :: term()}
			| ignore,
	State :: term(),
	Timeout :: non_neg_integer() | infinity.
init([]) ->
	BcManagerSup = whereis(bc_manager_sup),
	{ok, #state{manager_sup = BcManagerSup, games = dict:new()}}.

-spec handle_call(Request :: term(), From :: {pid(), Tag :: term()}, State :: term()) -> Result when
	Result :: {reply, Reply, NewState}
			| {reply, Reply, NewState, Timeout}
			| {reply, Reply, NewState, hibernate}
			| {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason, Reply, NewState}
			| {stop, Reason, NewState},
	Reply :: term(),
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity,
	Reason :: term().
handle_call({create_game, Privacy}, _From, 
	#state{manager_sup = BcManagerSup, games = GameDict} = State) ->
	case bc_game_model:save(Privacy, ?PENDING) of
		{ok, GameId} ->
			{ok, BcGameSup} = supervisor:start_child(BcManagerSup, #{
				id => GameId,
				start => {bc_game_sup, start_link, []},
				type => supervisor,
				restart => transient,
				shutdown => 5000,
				modules => [bc_game_sup]
			}),
			{ok, GameEventPid} = supervisor:start_child(BcGameSup, #{
				id => bc_game_event,
				start => {gen_event, start_link, []},
				restart => transient,
				shutdown => brutal_kill,
				modules => [gen_event]
			}),
			{ok, BcGameFsm} = supervisor:start_child(BcGameSup, #{
				id => bc_game_fsm,
				start => {bc_game_fsm, start_link, [GameId, GameEventPid, BcGameSup]},
				restart => transient,
				shutdown => 10000,
				modules => [bc_game_fsm]
			}),
			Ref = erlang:monitor(process, BcGameFsm),
			BcGame = bc_game:create(GameId, GameEventPid, BcGameFsm),
			GameMap = #{game => BcGame, sup => BcGameSup, monitor => Ref},
 			{reply, {ok, GameId, BcGameFsm}, 
			 	State#state{games = 
								dict:store(GameId, GameMap, GameDict)}};
		{error, Reason} ->
			{reply, {error, Reason}, State}
	end;
handle_call({get_game, GameId}, _From, State) ->
	GameDict = State#state.games,
	case dict:find(GameId, GameDict) of
		{ok, GameMap} ->
			BcGame = maps:get(game, GameMap),
			BcGameFsm = bc_game:fsm(BcGame),
			{reply, {ok, BcGameFsm}, State};
		error ->
			{reply, {error, not_found}, State}
	end.

-spec handle_cast(Request :: term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_cast({remove_game, GameId}, State) ->
	GameDict = State#state.games,
	{noreply, State#state{games = dict:erase(GameId, GameDict)}}.

-spec handle_info(Info :: timeout | term(), State :: term()) -> Result when
	Result :: {noreply, NewState}
			| {noreply, NewState, Timeout}
			| {noreply, NewState, hibernate}
			| {stop, Reason :: term(), NewState},
	NewState :: term(),
	Timeout :: non_neg_integer() | infinity.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, 
				#state{games = GameDict} = State) ->
	Games = dict:to_list(GameDict),
	case lists:filter(
			fun({_, GameMap}) -> 
				Ref =:= maps:get(monitor, GameMap)
			end, Games) of
		[{GameId, FoundGameMap}] ->
			%% TODO don't kill sup if exit in error
			BcGameSup = maps:get(sup, FoundGameMap),
			exit(BcGameSup, kill),
			BcGameFsm = maps:get(game, FoundGameMap),
			{noreply, State#state{games = dict:erase(GameId, GameDict)}};
		_ ->
			{noreply, State}
	end;
handle_info(Info, State) ->
    {noreply, State}.

-spec terminate(Reason, State :: term()) -> Any :: term() when
	Reason :: normal
			| shutdown
			| {shutdown, term()}
			| term().
terminate(Reason, State) ->
	io:format("BcManagerServer terminates with ~p~n", [Reason]),
    ok.

-spec code_change(OldVsn, State :: term(), Extra :: term()) -> Result when
	Result :: {ok, NewState :: term()} | {error, Reason :: term()},
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
code_change(OldVsn, State, Extra) ->
    {ok, State}.
