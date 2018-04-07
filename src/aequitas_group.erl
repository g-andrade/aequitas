%% Copyright (c) 2018 Guilherme Andrade
%%
%% Permission is hereby granted, free of charge, to any person obtaining a
%% copy  of this software and associated documentation files (the "Software"),
%% to deal in the Software without restriction, including without limitation
%% the rights to use, copy, modify, merge, publish, distribute, sublicense,
%% and/or sell copies of the Software, and to permit persons to whom the
%% Software is furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

-module(aequitas_group).

%% @reference <a target="_parent" href="https://en.wikipedia.org/wiki/Standard_score">Standard Score / Z-Score</a>

% https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/1,
    ask/3,
    set_settings/2,
    validate_settings/1,
    reload_settings/1
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

-define(DEFAULT_MAX_WINDOW_SIZE, infinity).
-define(DEFAULT_MAX_WINDOW_DURATION, 1000).

-define(DEFAULT_EVENT_WEIGHT, 1).
-define(DEFAULT_MAX_DEVIATION, 3).

-define(is_nonneg_integer(V), (is_integer((V)) andalso ((V) >= 0))).
-define(is_pos_integer(V), (is_integer((V)) andalso ((V) > 0))).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(settings, {
          max_window_size :: non_neg_integer() | infinity,
          max_window_duration :: non_neg_integer() | infinity
         }).
-type settings() :: #settings{}.

-record(event, {
          id :: term(),
          weight :: pos_integer(),
          timestamp :: integer()
         }).
-type event() :: #event{}.

-record(work_stats, {
          sum = 0 :: non_neg_integer(),
          squared_sum = 0 :: non_neg_integer(),
          mean = 0.0 :: float(),
          stddev = 0.0 :: float()
         }).
-type work_stats() :: #work_stats{}.

-record(state, {
          group :: atom(),
          settings :: settings(),
          %%
          window :: queue:queue(event()),
          window_size :: non_neg_integer(), % queue:len/1 is expensive
          %%
          work_shares :: #{ term() => pos_integer() },
          work_stats :: work_stats()
         }).
-type state() :: #state{}.

-type setting_opt() ::
        {max_window_size, non_neg_integer() | infinity} |
        {max_window_duration, non_neg_integer() | infinity}.
-export_type([setting_opt/0]).

-type ask_opt() ::
        {weight, pos_integer()} |
        {max_deviation, number()}.
-export_type([ask_opt/0]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start_link(atom()) -> {ok, pid()} | {error, already_started}.
%% @private
start_link(Group) ->
    proc_lib:start_link(?MODULE, init, [{self(), [Group]}]).

-spec ask(atom(), term(), [ask_opt()])
        -> {accepted | refused, #{ deviation => number() }} |
           {error, {process_down, term()}}.
%% @private
ask(Group, Id, Opts) ->
    Weight = proplists:get_value(weight, Opts, ?DEFAULT_EVENT_WEIGHT),
    MaxDeviation = proplists:get_value(max_deviation, Opts, ?DEFAULT_MAX_DEVIATION),
    ask(Group, Id, Weight, MaxDeviation).

-spec set_settings(atom(), [setting_opt()]) -> ok | {error, bad_settings}.
%% @private
set_settings(Group, SettingOpts) ->
    case validate_settings(SettingOpts) of
        ok ->
            aequitas_cfg:set({group, Group}, SettingOpts),
            reload_settings(Group);
        {error, Reason} ->
            {error, Reason}
    end.

-spec validate_settings([setting_opt()]) -> ok | {error, bad_settings}.
%% @private
validate_settings(SettingOpts) ->
    try settings(SettingOpts) of
        #settings{} ->
            ok
    catch
        _Class:_Reason ->
            {error, bad_settings}
    end.

-spec reload_settings(atom()) -> ok.
%% @private
reload_settings(Group) ->
    Server = server_name(Group),
    case whereis(Server) of
        undefined -> ok;
        ServerPid ->
            ServerPid ! reload_settings,
            ok
    end.

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init({pid(), [atom(), ...]}) -> no_return().
%% @private
init({Parent, [Group]}) ->
    Debug = sys:debug_options([]),
    Server = server_name(Group),
    try register(Server, self()) of
        true ->
            Settings = load_settings(Group),
            proc_lib:init_ack(Parent, {ok, self()}),
            State =
                #state{
                   group = Group,
                   settings = Settings,
                   window = queue:new(),
                   window_size = 0,
                   work_shares = #{},
                   work_stats = #work_stats{}
                  },
            loop(Parent, Debug, State)
    catch
        error:badarg ->
            proc_lib:init_ack(Parent, {error, already_started})
    end.

-spec write_debug(io:device(), term(), term()) -> ok.
%% @private
write_debug(Dev, Event, Name) ->
    % called by sys:handle_debug().
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

-spec system_continue(pid(), [sys:debug_opt()], state()) -> no_return().
%% @private
system_continue(Parent, Debug, State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_continue-3
    loop(Parent, Debug, State).

-spec system_terminate(term(), pid(), [sys:debug_opt()], state()) -> no_return().
%% @private
system_terminate(Reason, _Parent, _Debug, _State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_terminate-4
    exit(Reason).

-spec system_code_change(state(), ?MODULE, term(), term()) -> {ok, state()}.
%% http://www.erlang.org/doc/man/sys.html#Mod:system_code_change-4
%% @private
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------
%% Internal Functions Definitions - Initialization and Requests
%%-------------------------------------------------------------------

server_name(Group) ->
    list_to_atom(
      atom_to_list(?MODULE)
      ++ "."
      ++ atom_to_list(Group)).

load_settings(Group) ->
    SettingOpts = aequitas_cfg:get({group, Group}, []),
    try
        settings(SettingOpts)
    catch
        _Class:_Reason ->
            error({bad_settings, SettingOpts})
    end.

settings(SettingOpts) ->
    MaxWindowSize =
        proplists:get_value(max_window_size, SettingOpts, ?DEFAULT_MAX_WINDOW_SIZE),
    MaxWindowDuration =
        proplists:get_value(max_window_duration, SettingOpts, ?DEFAULT_MAX_WINDOW_DURATION),
    settings(MaxWindowSize, MaxWindowDuration).

settings(MaxWindowSize, MaxWindowDuration)
  when (?is_nonneg_integer(MaxWindowSize) orelse MaxWindowSize =:= infinity),
       (?is_nonneg_integer(MaxWindowDuration) orelse MaxWindowSize =:= infinity) ->
    #settings{
       max_window_size = MaxWindowSize,
       max_window_duration = MaxWindowDuration
      }.

ask(Group, Id, Weight, MaxDeviation)
  when is_atom(Group), ?is_pos_integer(Weight), is_number(MaxDeviation) ->
    call(Group, {ask, Id, Weight, MaxDeviation});
ask(_Group, _Id, _Weight, _MaxDeviation) ->
    error(badarg).

call(Group, Content) ->
    {ok, ServerPid} = ensure_server(Group),
    ServerMon = monitor(process, ServerPid),
    CallRef = ServerMon, % reuse ref
    ServerPid ! {call, {self(), CallRef}, Content},
    receive
        {reply, CallRef, Reply} ->
            demonitor(ServerMon, [flush]),
            Reply;
        {'DOWN', ServerMon, process, _Pid, Reason}
          when Reason =:= normal; Reason =:= noproc ->
            call(Group, Content);
        {'DOWN', ServerMon, process, _Pid, Reason} ->
            {error, {process_down, Reason}}
    end.

ensure_server(Group) ->
    Server = server_name(Group),
    case whereis(Server) of
        undefined ->
            case aequitas_group_sup:start_child([Group]) of
                {ok, Pid} ->
                    {ok, Pid};
                {error, already_started} ->
                    ensure_server(Group)
            end;
        Pid ->
            {ok, Pid}
    end.

%%-------------------------------------------------------------------
%% Internal Functions Definitions - Execution Loop
%%-------------------------------------------------------------------

loop(Parent, Debug, State) ->
    case loop_action(State) of
        simple ->
            receive
                Msg ->
                    handle_msg(Msg, Parent, Debug, State)
            end;
        {drop, Event} ->
            UpdatedState = drop_event(Event, State),
            loop(Parent, Debug, UpdatedState);
        {drop_after, Event, WaitTime} ->
            receive
                Msg ->
                    handle_msg(Msg, Parent, Debug, State)
            after
                WaitTime ->
                    UpdatedState = drop_event(Event, State),
                    loop(Parent, Debug, UpdatedState)
            end;
        {stop_after, WaitTime} ->
            receive
                Msg ->
                    handle_msg(Msg, Parent, Debug, State)
            after
                WaitTime ->
                    exit(normal)
            end
    end.

loop_action(State) ->
    EventPeek = queue:peek(State#state.window),
    Settings = State#state.settings,
    loop_action(EventPeek, Settings, State).

loop_action({value, Event}, Settings, State)
  when Settings#settings.max_window_size < State#state.window_size ->
    {drop, Event};
loop_action({value, Event}, Settings, _State)
  when Settings#settings.max_window_duration =/= infinity ->
    Now = erlang:monotonic_time(milli_seconds),
    ExpirationTs = Event#event.timestamp + Settings#settings.max_window_duration,
    WaitTime = ExpirationTs - Now,
    case WaitTime =< 0 of
        true ->
            {drop, Event};
        _ ->
            {drop_after, Event, WaitTime}
    end;
loop_action(empty, Settings, _State)
  when Settings#settings.max_window_duration =/= infinity ->
    IdleTimeout = Settings#settings.max_window_duration,
    {stop_after, IdleTimeout};
loop_action(_Event, _Settings, _State) ->
    simple.

drop_event(Event, State) ->
    UpdatedWindow = queue:drop(State#state.window),
    UpdatedWindowSize = State#state.window_size - 1,
    {PrevShare, UpdatedShare, UpdatedWorkShares} =
        update_work_share(Event#event.id, -Event#event.weight, State#state.work_shares),
    UpdatedWorkStats =
        update_work_stats(PrevShare, UpdatedShare, UpdatedWorkShares, State#state.work_stats),
    State#state{
      window = UpdatedWindow,
      window_size = UpdatedWindowSize,
      work_shares = UpdatedWorkShares,
      work_stats = UpdatedWorkStats
     }.

handle_msg({system, From, Request}, Parent, Debug, State) ->
    sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
handle_msg(Msg, Parent, Debug, State) ->
    UpdatedDebug = sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}),
    handle_nonsystem_msg(Msg, Parent, UpdatedDebug, State).

handle_nonsystem_msg({call, From, Content}, Parent, Debug, State) ->
    {FromPid, CallRef} = From,
    {Reply, UpdatedState} = handle_call(Content, State),
    FromPid ! {reply, CallRef, Reply},
    loop(Parent, Debug, UpdatedState);
handle_nonsystem_msg(reload_settings, Parent, Debug, State) ->
    UpdatedState = handle_settings_reload(State),
    loop(Parent, Debug, UpdatedState);
handle_nonsystem_msg(Msg, _Parent, _Debug, _State) ->
    error({unexpected_msg, Msg}).

handle_call({ask, Id, Weight, MaxDeviation}, State) ->
    Deviation = deviation(Id, State),
    case Deviation >= MaxDeviation of
        true ->
            {{refused, #{ deviation => Deviation }}, State};
        _ ->
            Event =
                #event{
                   id = Id,
                   weight = Weight,
                   timestamp = erlang:monotonic_time(milli_seconds)
                  },

            UpdatedWindow = queue:in(Event, State#state.window),
            UpdatedWindowSize = State#state.window_size + 1,
            {PrevShare, UpdatedShare, UpdatedWorkShares} =
                update_work_share(Event#event.id, Weight, State#state.work_shares),
            UpdatedWorkStats =
                update_work_stats(PrevShare, UpdatedShare, UpdatedWorkShares, State#state.work_stats),
            UpdatedState =
                State#state{
                  window = UpdatedWindow,
                  window_size = UpdatedWindowSize,
                  work_shares = UpdatedWorkShares,
                  work_stats = UpdatedWorkStats
                 },
            {{accepted, #{ deviation => Deviation }}, UpdatedState}
    end.

handle_settings_reload(State) ->
    State#state{
      settings = load_settings(State#state.group)
     }.

deviation(Id, State) ->
    % Standard score / Z-score:
    % https://en.wikipedia.org/wiki/Standard_score
    WorkStats = State#state.work_stats,
    case WorkStats#work_stats.stddev of
        0.0 ->
            0.0;
        StdDev ->
            Share = maps:get(Id, State#state.work_shares, 0),
            (Share - WorkStats#work_stats.mean) / StdDev
    end.

update_work_share(Id, ShareIncr, WorkShares) ->
    ShareLookup = maps:find(Id, WorkShares),
    update_work_share(Id, ShareLookup, ShareIncr, WorkShares).

update_work_share(Id, {ok, Share}, ShareIncr, WorkShares) ->
    update_existing_work_share(Id, Share, Share + ShareIncr, WorkShares);
update_work_share(Id, error, ShareIncr, WorkShares) ->
    UpdatedWorkShares = maps:put(Id, ShareIncr, WorkShares),
    {0, ShareIncr, UpdatedWorkShares}.

update_existing_work_share(Id, Share, UpdatedShare, WorkShares) when UpdatedShare =:= 0 ->
    UpdatedWorkShares = maps:remove(Id, WorkShares),
    {Share, UpdatedShare, UpdatedWorkShares};
update_existing_work_share(Id, Share, UpdatedShare, WorkShares) ->
    UpdatedWorkShares = maps:update(Id, UpdatedShare, WorkShares),
    {Share, UpdatedShare, UpdatedWorkShares}.

update_work_stats(PrevShare, UpdatedShare, UpdatedWorkShares, WorkStats) ->
    UpdatedWorkSharesSize = maps:size(UpdatedWorkShares),
    case UpdatedWorkSharesSize =:= 0 of
        true ->
            #work_stats{};
        _ ->
            UpdatedSum = WorkStats#work_stats.sum - PrevShare + UpdatedShare,
            UpdatedSquaredSum = (WorkStats#work_stats.squared_sum
                                 - (PrevShare * PrevShare)
                                 + (UpdatedShare * UpdatedShare)),
            UpdatedMean = UpdatedSum / UpdatedWorkSharesSize,
            UpdatedVariance = ((UpdatedSquaredSum / UpdatedWorkSharesSize)
                               - (UpdatedMean * UpdatedMean)),
            UpdatedStdDev = math:sqrt(UpdatedVariance),
            #work_stats{
               sum = UpdatedSum,
               squared_sum = UpdatedSquaredSum,
               mean = UpdatedMean,
               stddev = UpdatedStdDev
              }
    end.
