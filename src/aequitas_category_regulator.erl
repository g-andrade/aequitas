-module(aequitas_category_regulator).

% https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/1
   ]).

-ignore_xref(
   [start_link/1
   ]).

%%-------------------------------------------------------------------
%% OTP Function Exports
%%-------------------------------------------------------------------

-export(
   [init/1,
    system_code_change/4,
    system_continue/3,
    system_terminate/4,
    write_debug/3
   ]).

-ignore_xref(
   [init/1,
    system_code_change/4,
    system_continue/3,
    system_terminate/4,
    write_debug/3
   ]).

%%-------------------------------------------------------------------
%% Macro Definitions
%%-------------------------------------------------------------------

-define(SERVER, ?MODULE).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          category :: term(),
          window :: queue:queue(timestamp()),
          window_sz :: non_neg_integer(), % queue:len/1 is expensive
          max_rate :: non_neg_integer(),
          broker_pid :: pid(),
          broker_tag :: undefined | reference()
         }).
-type state() :: #state{}.

-type timestamp() :: integer().

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start_link(term()) -> {ok, pid()}.
%% @private
start_link(Category) ->
    proc_lib:start_link(?MODULE, init, [{self(), [Category]}]).

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init({pid(), [term(), ...]}) -> no_return().
%% @private
init({Parent, [Category]}) ->
    register(?SERVER, self()),
    Debug = sys:debug_options([]),
    proc_lib:init_ack(Parent, {ok, self()}),
    BrokerPid = aequitas_categories_manager:ensure_broker(Category),
    State =
        #state{ category = Category,
                window = queue:new(),
                window_sz = 0,
                max_rate = infinity,
                broker_pid = BrokerPid
              },
    UpdatedState = maybe_async_ask_r(State),
    loop(Parent, Debug, UpdatedState).

-spec write_debug(io:device(), term(), term()) -> ok.
%% @private
write_debug(Dev, Event, Name) ->
    % called by sys:handle_debug().
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

-spec system_continue(pid(), [sys:debug_opt()], state()) -> no_return().
%% @private
system_continue(Parent, Debug, State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_continue-3
    % io:format("Continue!~n"),
    loop(Parent, Debug, State).

-spec system_terminate(term(), pid(), [sys:debug_opt()], state()) -> no_return().
%% @private
system_terminate(Reason, _Parent, _Debug, _State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_terminate-4
    % io:format("Terminate!~n"),
    exit(Reason).

-spec system_code_change(state(), ?MODULE, term(), term()) -> {ok, state()}.
%% http://www.erlang.org/doc/man/sys.html#Mod:system_code_change-4
%% @private
system_code_change(State, _Module, _OldVsn, _Extra) ->
    %io:format("Changed code!~n"),
    {ok, State}.

%%-------------------------------------------------------------------
%% Internal Functions Definitions
%%-------------------------------------------------------------------

loop(Parent, Debug, State) ->
    LoopAction = loop_action(State),
    loop(LoopAction, Parent, Debug, State).

loop(simple, Parent, Debug, State) ->
    receive
        Msg ->
            handle_msg(Msg, Parent, Debug, State)
    end;
loop(shrink_window, Parent, Debug, State) ->
    UpdatedState = shrink_window(State),
    loop(Parent, Debug, UpdatedState);
loop({shrink_window_after, Timeout}, Parent, Debug, State) ->
    receive
        Msg ->
            handle_msg(Msg, Parent, Debug, State)
    after
        Timeout ->
            UpdatedState = shrink_window(State),
            loop(Parent, Debug, UpdatedState)
    end.

loop_action(State) ->
    case queue:peek(State#state.window) of
        {value, ExpirationTs} ->
            TimeRemaining = ExpirationTs - now_ms(),
            case TimeRemaining > 0 of
                true ->
                    {shrink_window_after, TimeRemaining};
                false ->
                    shrink_window
            end;
        empty ->
            simple
    end.

handle_msg({system, From, Request}, Parent, Debug, State) ->
    sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
handle_msg(Msg, Parent, Debug, State) ->
    UpdatedDebug = sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}),
    UpdatedState = handle_regular_msg(Msg, State),
    loop(Parent, UpdatedDebug, UpdatedState).

handle_regular_msg({Tag, BrokerResult}, State) when Tag =:= State#state.broker_tag ->
    UpdatedState = State#state{ broker_tag = undefined },
    handle_broker_result(BrokerResult, UpdatedState).

handle_broker_result({go, _Ref, _Actor, _RelativeTime, _SojournTime}, State) ->
    Window = State#state.window,
    WindowSz = State#state.window_sz,
    ExpirationTs = now_ms() + 1000,
    UpdatedWindow = queue:in(ExpirationTs, Window),
    UpdatedWindowSz = WindowSz + 1,
    UpdatedState = State#state{ window = UpdatedWindow, window_sz = UpdatedWindowSz },
    maybe_async_ask_r(UpdatedState);
handle_broker_result({drop, _SojournTime}, State) ->
    maybe_async_ask_r(State).

shrink_window(State) ->
    Window = State#state.window,
    WindowSz = State#state.window_sz,
    UpdatedState =
        State#state{ window = queue:drop(Window),
                     window_sz = WindowSz - 1
                   },
    maybe_async_ask_r(UpdatedState).

now_ms() ->
    erlang:monotonic_time(milli_seconds).

maybe_async_ask_r(State) when State#state.window_sz >= State#state.max_rate ->
    State;
maybe_async_ask_r(State) when State#state.broker_tag =/= undefined ->
    State;
maybe_async_ask_r(State) ->
    {await, Tag, _Process} = aequitas_category_broker:async_ask_r(State#state.broker_pid),
    State#state{ broker_tag = Tag }.
