-module(aequitas_category_broker).
-behaviour(sbroker).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/1,
    ask/2,
    async_ask_r/1
   ]).

%% ------------------------------------------------------------------
%% sbroker Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1
   ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Category) ->
    sbroker:start_link(?MODULE, [Category], []).

ask(BrokerPid, Actor) ->
    sbroker:ask(BrokerPid, Actor).

async_ask_r(BrokerPid) ->
    %io:format("~p async_ask_r(~p)~n", [self(), BrokerPid]),
    sbroker:async_ask_r(BrokerPid).

%% ------------------------------------------------------------------
%% sbroker Function Definitions
%% ------------------------------------------------------------------

init([Category]) ->
    true = aequitas_categories_manager:update_broker_pid(Category, self()),
    AskQueueSpec = ask_queue_spec(),
    AskRQueueSpec = ask_r_queue_spec(),
    MeterSpec = {sbroker_overload_meter, #{alarm => {overload, ?MODULE}}},
    {ok, {AskQueueSpec, AskRQueueSpec, [MeterSpec]}}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

ask_queue_spec() ->
    FairnessKey = value,
    FairnessIndex = FairnessKey,
    {sbroker_fair_queue, {sbroker_codel_queue, #{}, FairnessIndex}}.

ask_r_queue_spec() ->
    {sbroker_drop_queue, #{}}.
