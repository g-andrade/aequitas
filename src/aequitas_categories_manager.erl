-module(aequitas_categories_manager).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/0,
    ensure_broker/1,
    update_broker_pid/2
   ]).

-ignore_xref(
   [start_link/0
   ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
          tracker_sup_monitors :: #{ reference() => term() }
         }).

-record(tracker, {
          category :: term(),
          sup_pid :: undefined | pid(),
          broker_pid :: undefined | pid()
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local,?SERVER}, ?CB_MODULE, [], []).

ensure_broker(Category) ->
    Tracker = ensure_tracker(Category),
    Tracker#tracker.broker_pid.

update_broker_pid(Category, BrokerPid) ->
    Update = {#tracker.broker_pid, BrokerPid},
    ets:update_element(?TABLE, Category, Update).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([]) ->
    EtsOpts = [named_table, public, {keypos,#tracker.category},
               {read_concurrency,true}],
    _ = ets:new(?TABLE, EtsOpts),
    {ok, #state{ tracker_sup_monitors = #{} }}.

handle_call({upsert_tracker, Category}, _From, State) ->
    UpdatedState = maybe_spawn_tracker(Category, State),
    [Tracker] = ets:lookup(?TABLE, Category),
    {reply, Tracker, UpdatedState};
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    Monitors = State#state.tracker_sup_monitors,
    {Category, UpdatedMonitors} = maps:take(Ref, Monitors),
    [#tracker{}] = ets:take(?TABLE, Category),
    UpdatedState = State#state{ tracker_sup_monitors = UpdatedMonitors },
    {noreply, UpdatedState};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ensure_tracker(Category) ->
    case ets:lookup(?TABLE, Category) of
        [Tracker] when Tracker#tracker.broker_pid =/= undefined -> 
            Tracker;
        _ -> 
            gen_server:call(?SERVER, {upsert_tracker, Category}, infinity)
    end.

maybe_spawn_tracker(Category, State) ->
    case ets:insert_new(?TABLE, #tracker{ category = Category }) of
        false ->
            State;
        true ->
            spawn_tracker(Category, State)
    end.

spawn_tracker(Category, State) ->
    {ok, SupPid} = aequitas_category_sup:start(Category),
    Update = {#tracker.sup_pid, SupPid},
    true = ets:update_element(?TABLE, Category, Update),
    SupMon = monitor(process, SupPid),
    OtherMonitors = State#state.tracker_sup_monitors,
    UpdatedMonitors = maps:put(SupMon, Category, OtherMonitors),
    State#state{ tracker_sup_monitors = UpdatedMonitors }.
