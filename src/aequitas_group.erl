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

%% @reference <a target="_parent" href="https://en.wikipedia.org/wiki/Standard_score">Standard Score / Z-Score</a>

-module(aequitas_group).

% https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/1,
    ask/3,
    async_ask/3,
    set_settings/2,
    validate_settings/1,
    reload_settings/1,
    async_reload_settings/1
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

-define(DEFAULT_MAX_WINDOW_SIZE, 10000).
-define(DEFAULT_MAX_WINDOW_DURATION, 5000).

-define(DEFAULT_EVENT_WEIGHT, 1).
-define(DEFAULT_MAX_ZSCORE, 3).

-define(is_pos_integer(V), (is_integer((V)) andalso ((V) > 0))).

%%-------------------------------------------------------------------
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(settings, {
          max_window_size :: pos_integer() | infinity,
          max_window_duration :: pos_integer() | infinity
         }).
-type settings() :: #settings{}.

-record(event, {
          id :: term(),
          weight :: pos_integer(),
          timestamp :: integer()
         }).
-type event() :: #event{}.

-record(work_stats, {
          sum = 0 :: non_neg_integer(), % used to calculate mean
          squared_sum = 0 :: non_neg_integer(), % used to calculate stddev
          mean = 0.0 :: float(), % used to calculate stddev and z-score
          stddev = 0.0 :: float() % used to calculate z-score
         }).
-type work_stats() :: #work_stats{}.

-record(state, {
          group :: atom(), % the group identifier
          settings :: settings(), % the group settings
          %%
          window :: queue:queue(event()), % the event window
          window_size :: non_neg_integer(), % queue:len/1 is expensive
          %%
          work_shares :: #{ term() => pos_integer() }, % work share per actor
          work_stats :: work_stats() % used to calculate z-score
         }).
-type state() :: #state{}.

-record(ask_params, {
          weight :: pos_integer(),
          max_zscore :: number() | infinity
         }).

-type setting_opt() ::
        {max_window_size, pos_integer() | infinity} |
        {max_window_duration, pos_integer() | infinity}.
-export_type([setting_opt/0]).

-type ask_opt() ::
        {weight, pos_integer()} |
        {max_zscore, number() | infinity}.
-export_type([ask_opt/0]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start_link(atom()) -> {ok, pid()} | {error, already_started}.
%% @private
start_link(Group) ->
    proc_lib:start_link(?MODULE, init, [{self(), [Group]}]).

-spec ask(atom() | pid(), term(), [ask_opt()]) -> accepted | rejected.
%% @private
ask(Group, Actor, Opts) when is_atom(Group) ->
    Pid = ensure_server(Group),
    ask(Pid, Actor, Opts);
ask(Pid, Actor, Opts) when is_pid(Pid) ->
    {Tag, Mon} = async_ask(Pid, Actor, Opts),
    wait_call_reply(Tag, Mon).

-spec async_ask(atom() | pid(), term(), [ask_opt()]) -> {reference(), reference()}.
%% @private
async_ask(Group, Actor, Opts) when is_atom(Group) ->
    Pid = ensure_server(Group),
    async_ask(Pid, Actor, Opts);
async_ask(Pid, Actor, Opts) when is_pid(Pid) ->
    Params = parse_ask_opts(Opts),
    send_call(Pid, {ask, Actor, Params}).

-spec set_settings(atom(), [setting_opt()])
        -> ok | {error, {invalid_setting_opt | invalid_setting_opts, _}}.
%% @private
set_settings(Group, SettingOpts) when is_atom(Group) ->
    case validate_settings(SettingOpts) of
        ok ->
            aequitas_cfg:set({group, Group}, SettingOpts),
            reload_settings(Group);
        {error, Reason} ->
            {error, Reason}
    end.

-spec validate_settings([setting_opt()]) -> ok | {error, term()}.
%% @private
validate_settings(SettingOpts) ->
    case parse_settings_opts(SettingOpts) of
        {ok, _Settings} ->
            ok;
        {error, Reason} ->
            {error, Reason}
    end.

-spec reload_settings(atom()) -> ok.
%% @private
reload_settings(Group) when is_atom(Group) ->
    Pid = ensure_server(Group),
    {Tag, Mon} = send_call(Pid, reload_settings),
    wait_call_reply(Tag, Mon).

-spec async_reload_settings(atom()) -> ok.
%% @private
async_reload_settings(Group) when is_atom(Group) ->
    Pid = ensure_server(Group),
    send_cast(Pid, reload_settings).

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

ensure_server(Group) ->
    Server = server_name(Group),
    case whereis(Server) of
        undefined ->
            case aequitas_group_sup:start_child([Group]) of
                {ok, Pid} ->
                    Pid;
                {error, already_started} ->
                    whereis(Server)
            end;
        Pid ->
            Pid
    end.

server_name(Group) ->
    list_to_atom(
      atom_to_list(?MODULE)
      ++ "."
      ++ atom_to_list(Group)).

load_settings(Group) ->
    SettingOpts = aequitas_cfg:get({group, Group}, []),
    case parse_settings_opts(SettingOpts) of
        {ok, Settings} ->
            Settings;
        {error, Reason} ->
            error(#{ group => Group, reason => Reason })
    end.

parse_settings_opts(SettingOpts) ->
    DefaultSettings =
        #settings{ max_window_size = ?DEFAULT_MAX_WINDOW_SIZE,
                   max_window_duration = ?DEFAULT_MAX_WINDOW_DURATION
                 },
    parse_settings_opts(SettingOpts, DefaultSettings).

parse_settings_opts([{max_window_size, MaxWindowSize} | Next], Acc)
  when ?is_pos_integer(MaxWindowSize); MaxWindowSize =:= infinity ->
    parse_settings_opts(Next, Acc#settings{ max_window_size = MaxWindowSize });
parse_settings_opts([{max_window_duration, MaxWindowDuration} | Next], Acc)
  when ?is_pos_integer(MaxWindowDuration); MaxWindowDuration =:= infinity ->
    parse_settings_opts(Next, Acc#settings{ max_window_duration = MaxWindowDuration });
parse_settings_opts([], Acc) ->
    {ok, Acc};
parse_settings_opts([InvalidOpt | _Next], _Acc) ->
    {error, {invalid_setting_opt, InvalidOpt}};
parse_settings_opts(InvalidOpts, _Acc) ->
    {error, {invalid_setting_opts, InvalidOpts}}.

send_call(Pid, Call) ->
    Mon = monitor(process, Pid),
    Tag = Mon,
    Pid ! {call, self(), Tag, Call},
    {Tag, Mon}.

send_cast(Pid, Cast) ->
    Pid ! {cast, Cast},
    ok.

wait_call_reply(Tag, Mon) ->
    receive
        {Tag, Reply} ->
            demonitor(Mon, [flush]),
            Reply;
        {'DOWN', Mon, process, _Pid, Reason} ->
            error({group_process_stopped, Reason})
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

handle_nonsystem_msg({call, Pid, Tag, {ask, Actor, Params}}, Parent, Debug, State) ->
    {Reply, UpdatedState} = handle_ask(Actor, Params, State),
    Pid ! {Tag, Reply},
    loop(Parent, Debug, UpdatedState);
handle_nonsystem_msg({call, Pid, Tag, reload_settings}, Parent, Debug, State) ->
    UpdatedState = handle_settings_reload(State),
    Pid ! {Tag, ok},
    loop(Parent, Debug, UpdatedState);
handle_nonsystem_msg({cast, reload_settings}, Parent, Debug, State) ->
    UpdatedState = handle_settings_reload(State),
    loop(Parent, Debug, UpdatedState);
handle_nonsystem_msg(Msg, _Parent, _Debug, _State) ->
    error({unexpected_msg, Msg}).

handle_settings_reload(State) ->
    State#state{
      settings = load_settings(State#state.group)
     }.

%%-------------------------------------------------------------------
%% Internal Functions Definitions - Asking
%%-------------------------------------------------------------------

parse_ask_opts(Opts) ->
    DefaultParams =
        #ask_params{
           weight = ?DEFAULT_EVENT_WEIGHT,
           max_zscore = ?DEFAULT_MAX_ZSCORE
          },
    parse_ask_opts(Opts, DefaultParams).

parse_ask_opts([{weight, Weight} | Next], Acc)
  when ?is_pos_integer(Weight) ->
    parse_ask_opts(Next, Acc#ask_params{ weight = Weight });
parse_ask_opts([{max_zscore, MaxZScore} | Next], Acc)
  when is_number(MaxZScore); MaxZScore =:= infinity ->
    parse_ask_opts(Next, Acc#ask_params{ max_zscore = MaxZScore });
parse_ask_opts([], Acc) ->
    Acc;
parse_ask_opts(InvalidOpts, _Acc) ->
    error({badarg, InvalidOpts}).

handle_ask(Actor, Params, State) ->
    ZScore = zscore(Actor, State),
    case ZScore =< Params#ask_params.max_zscore of
        true ->
            UpdatedState = accept(Actor, Params, State),
            {accepted, UpdatedState};
        _ ->
            {rejected, State}
    end.

accept(Actor, Params, State)
  when State#state.window_size >= (State#state.settings)#settings.max_window_size ->
    {value, Event} = queue:peek(State#state.window),
    UpdatedState = drop_event(Event, State),
    accept(Actor, Params, UpdatedState);
accept(Actor, Params, State) ->
    Event =
        #event{
           id = Actor,
           weight = Params#ask_params.weight,
           timestamp = erlang:monotonic_time(milli_seconds)
          },

    UpdatedWindow = queue:in(Event, State#state.window),
    UpdatedWindowSize = State#state.window_size + 1,
    {PrevShare, UpdatedShare, UpdatedWorkShares} =
        update_work_share(Event#event.id, Params#ask_params.weight, State#state.work_shares),
    UpdatedWorkStats =
        update_work_stats(PrevShare, UpdatedShare, UpdatedWorkShares, State#state.work_stats),
    State#state{
      window = UpdatedWindow,
      window_size = UpdatedWindowSize,
      work_shares = UpdatedWorkShares,
      work_stats = UpdatedWorkStats
     }.

update_work_share(Actor, ShareIncr, WorkShares) ->
    ShareLookup = maps:find(Actor, WorkShares),
    update_work_share(Actor, ShareLookup, ShareIncr, WorkShares).

update_work_share(Actor, {ok, Share}, ShareIncr, WorkShares) ->
    update_existing_work_share(Actor, Share, Share + ShareIncr, WorkShares);
update_work_share(Actor, error, ShareIncr, WorkShares) ->
    UpdatedWorkShares = maps:put(Actor, ShareIncr, WorkShares),
    {0, ShareIncr, UpdatedWorkShares}.

update_existing_work_share(Actor, Share, UpdatedShare, WorkShares) when UpdatedShare =:= 0 ->
    UpdatedWorkShares = maps:remove(Actor, WorkShares),
    {Share, UpdatedShare, UpdatedWorkShares};
update_existing_work_share(Actor, Share, UpdatedShare, WorkShares) ->
    UpdatedWorkShares = maps:update(Actor, UpdatedShare, WorkShares),
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

zscore(Actor, State) ->
    % Standard score / Z-score:
    % https://en.wikipedia.org/wiki/Standard_score
    WorkStats = State#state.work_stats,
    case WorkStats#work_stats.stddev of
        0.0 ->
            0.0;
        StdDev ->
            WorkShare = maps:get(Actor, State#state.work_shares, 0),
            (WorkShare - WorkStats#work_stats.mean) / StdDev
    end.
