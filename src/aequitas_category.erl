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
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO WORK SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%% DEALINGS IN THE SOFTWARE.

%% @reference <a target="_parent" href="https://en.wikipedia.org/wiki/Standard_score">Standard Score / Z-Score</a> (Wikpiedia)
%% @reference <a target="_parent" href="http://www.statisticshowto.com/probability-and-statistics/hypothesis-testing/t-score-vs-z-score/">T-Score vs. Z-score</a> (statisticshowto.com)
%% @reference <a target="_parent" href="https://www.itl.nist.gov/div898/handbook/eda/section3/eda35h.htm">Detection of Outliers</a> (Wikipedia)
%% @reference <a target="_parent" href="https://en.wikipedia.org/wiki/Robust_measures_of_scale">Robust measures of scale</a> (Wikipedia)
%% @reference <a target="_parent" href="https://pkghosh.wordpress.com/2015/08/25/anomaly-detection-with-robust-zscore/">Anomaly Detection with Robust Zscore</a> (pkghosh.wordpress.com)

-module(aequitas_category).

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

-define(DEFAULT_WORK_WEIGHT, 1).
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

-record(work, {
          actor_id :: term(),
          weight :: pos_integer(),
          timestamp :: integer()
         }).
-type work() :: #work{}.

-record(work_quartiles, {
          tree = aequitas_quartiles:empty(),
          %gb_sets:new() :: gb_sets:set({pos_integer(), term()}), % work share per actor
          q2 :: undefined | {pos_integer(), term()},
          q1 :: undefined | {pos_integer(), term()},
          q3 :: undefined | {pos_integer(), term()}
         }).
-type work_quartiles() :: #work_quartiles{}.

-record(work_stats, {
          sum = 0 :: non_neg_integer(), % used to calculate mean
          squared_sum = 0 :: non_neg_integer(), % used to calculate stddev
          mean = 0.0 :: float(), % used to calculate stddev and z-score
          stddev = 0.0 :: float(),% used to calculate z-score
          quartiles = #work_quartiles{} :: work_quartiles()
         }).
-type work_stats() :: #work_stats{}.

-record(state, {
          category :: atom(), % the category identifier
          settings :: settings(), % the category settings
          %%
          window :: queue:queue(work()), % sliding window
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
start_link(Category) ->
    proc_lib:start_link(?MODULE, init, [{self(), [Category]}]).

-spec ask(atom() | pid(), term(), [ask_opt()]) -> accepted | rejected.
%% @private
ask(Category, ActorId, Opts) when is_atom(Category) ->
    Pid = ensure_server(Category),
    ask(Pid, ActorId, Opts);
ask(Pid, ActorId, Opts) when is_pid(Pid) ->
    {Tag, Mon} = async_ask(Pid, ActorId, Opts),
    wait_call_reply(Tag, Mon).

-spec async_ask(atom() | pid(), term(), [ask_opt()]) -> {reference(), reference()}.
%% @private
async_ask(Category, ActorId, Opts) when is_atom(Category) ->
    Pid = ensure_server(Category),
    async_ask(Pid, ActorId, Opts);
async_ask(Pid, ActorId, Opts) when is_pid(Pid) ->
    Params = parse_ask_opts(Opts),
    send_call(Pid, {ask, ActorId, Params}).

-spec set_settings(atom(), [setting_opt()])
        -> ok | {error, {invalid_setting_opt | invalid_setting_opts, _}}.
%% @private
set_settings(Category, SettingOpts) when is_atom(Category) ->
    case validate_settings(SettingOpts) of
        ok ->
            aequitas_cfg:set({category, Category}, SettingOpts),
            reload_settings(Category);
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
reload_settings(Category) when is_atom(Category) ->
    Pid = ensure_server(Category),
    {Tag, Mon} = send_call(Pid, reload_settings),
    wait_call_reply(Tag, Mon).

-spec async_reload_settings(atom()) -> ok.
%% @private
async_reload_settings(Category) when is_atom(Category) ->
    Pid = ensure_server(Category),
    send_cast(Pid, reload_settings).

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init({pid(), [atom(), ...]}) -> no_return().
%% @private
init({Parent, [Category]}) ->
    Debug = sys:debug_options([]),
    Server = server_name(Category),
    try register(Server, self()) of
        true ->
            Settings = load_settings(Category),
            proc_lib:init_ack(Parent, {ok, self()}),
            State =
                #state{
                   category = Category,
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

ensure_server(Category) ->
    Server = server_name(Category),
    case whereis(Server) of
        undefined ->
            case aequitas_category_sup:start_child([Category]) of
                {ok, Pid} ->
                    Pid;
                {error, already_started} ->
                    whereis(Server)
            end;
        Pid ->
            Pid
    end.

server_name(Category) ->
    list_to_atom(
      atom_to_list(?MODULE)
      ++ "."
      ++ atom_to_list(Category)).

load_settings(Category) ->
    SettingOpts = aequitas_cfg:get({category, Category}, []),
    case parse_settings_opts(SettingOpts) of
        {ok, Settings} ->
            Settings;
        {error, Reason} ->
            error(#{ category => Category, reason => Reason })
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
            error({category_process_stopped, Reason})
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
        {drop, Work} ->
            UpdatedState = drop_work(Work, State),
            loop(Parent, Debug, UpdatedState);
        {drop_after, Work, WaitTime} ->
            receive
                Msg ->
                    handle_msg(Msg, Parent, Debug, State)
            after
                WaitTime ->
                    UpdatedState = drop_work(Work, State),
                    loop(Parent, Debug, UpdatedState)
            end;
        {hibernate_after, WaitTime} ->
            receive
                Msg ->
                    handle_msg(Msg, Parent, Debug, State)
            after
                WaitTime ->
                    hibernate(Parent, Debug, State)
            end
    end.

loop_action(State) ->
    WorkPeek = queue:peek(State#state.window),
    Settings = State#state.settings,
    loop_action(WorkPeek, Settings, State).

loop_action({value, Work}, Settings, State)
  when Settings#settings.max_window_size < State#state.window_size ->
    {drop, Work};
loop_action({value, Work}, Settings, _State)
  when Settings#settings.max_window_duration =/= infinity ->
    Now = erlang:monotonic_time(milli_seconds),
    ExpirationTs = Work#work.timestamp + Settings#settings.max_window_duration,
    WaitTime = ExpirationTs - Now,
    case WaitTime =< 0 of
        true ->
            {drop, Work};
        _ ->
            {drop_after, Work, WaitTime}
    end;
loop_action(empty, Settings, _State)
  when Settings#settings.max_window_duration =/= infinity ->
    IdleTimeout = Settings#settings.max_window_duration,
    {hibernate_after, IdleTimeout};
loop_action(_Work, _Settings, _State) ->
    simple.

drop_work(Work, State) ->
    UpdatedWindow = queue:drop(State#state.window),
    UpdatedWindowSize = State#state.window_size - 1,
    {PrevShare, UpdatedShare, UpdatedWorkShares} =
        update_work_share(Work#work.actor_id, -Work#work.weight, State#state.work_shares),
    UpdatedWorkStats =
        update_work_stats(Work#work.actor_id, PrevShare, UpdatedShare, UpdatedWorkShares,
                          State#state.work_stats),
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

handle_nonsystem_msg({call, Pid, Tag, {ask, ActorId, Params}}, Parent, Debug, State) ->
    {Reply, UpdatedState} = handle_ask(ActorId, Params, State),
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
      settings = load_settings(State#state.category)
     }.

hibernate(Parent, Debug, State) ->
    proc_lib:hibernate(?MODULE, system_continue, [Parent, Debug, State]).

%%-------------------------------------------------------------------
%% Internal Functions Definitions - Asking
%%-------------------------------------------------------------------

parse_ask_opts(Opts) ->
    DefaultParams =
        #ask_params{
           weight = ?DEFAULT_WORK_WEIGHT,
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

handle_ask(ActorId, Params, State) ->
    ZScore = zscore(ActorId, State),
    case ZScore =< Params#ask_params.max_zscore of
        true ->
            UpdatedState = accept(ActorId, Params, State),
            {accepted, UpdatedState};
        _ ->
            {rejected, State}
    end.

accept(ActorId, Params, State)
  when State#state.window_size >= (State#state.settings)#settings.max_window_size ->
    {value, Work} = queue:peek(State#state.window),
    UpdatedState = drop_work(Work, State),
    accept(ActorId, Params, UpdatedState);
accept(ActorId, Params, State) ->
    Work =
        #work{
           actor_id = ActorId,
           weight = Params#ask_params.weight,
           timestamp = erlang:monotonic_time(milli_seconds)
          },

    UpdatedWindow = queue:in(Work, State#state.window),
    UpdatedWindowSize = State#state.window_size + 1,
    {PrevShare, UpdatedShare, UpdatedWorkShares} =
        update_work_share(Work#work.actor_id, Params#ask_params.weight, State#state.work_shares),
    UpdatedWorkStats =
        update_work_stats(Work#work.actor_id, PrevShare, UpdatedShare, UpdatedWorkShares,
                          State#state.work_stats),
    State#state{
      window = UpdatedWindow,
      window_size = UpdatedWindowSize,
      work_shares = UpdatedWorkShares,
      work_stats = UpdatedWorkStats
     }.

update_work_share(ActorId, ShareIncr, WorkShares) ->
    ShareLookup = maps:find(ActorId, WorkShares),
    update_work_share(ActorId, ShareLookup, ShareIncr, WorkShares).

update_work_share(ActorId, {ok, Share}, ShareIncr, WorkShares) ->
    update_existing_work_share(ActorId, Share, Share + ShareIncr, WorkShares);
update_work_share(ActorId, error, ShareIncr, WorkShares) ->
    UpdatedWorkShares = maps:put(ActorId, ShareIncr, WorkShares),
    {0, ShareIncr, UpdatedWorkShares}.

update_existing_work_share(ActorId, Share, UpdatedShare, WorkShares) when UpdatedShare =:= 0 ->
    UpdatedWorkShares = maps:remove(ActorId, WorkShares),
    {Share, UpdatedShare, UpdatedWorkShares};
update_existing_work_share(ActorId, Share, UpdatedShare, WorkShares) ->
    UpdatedWorkShares = maps:update(ActorId, UpdatedShare, WorkShares),
    {Share, UpdatedShare, UpdatedWorkShares}.

update_work_stats(ActorId, PrevShare, UpdatedShare, UpdatedWorkShares, WorkStats) ->
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
            UpdatedQuartiles = update_work_quartiles(ActorId, PrevShare, UpdatedShare,
                                                     WorkStats#work_stats.quartiles),
            #work_stats{
               sum = UpdatedSum,
               squared_sum = UpdatedSquaredSum,
               mean = UpdatedMean,
               stddev = UpdatedStdDev,
               quartiles = UpdatedQuartiles
              }
    end.

update_work_quartiles(ActorId, PrevShare, UpdatedShare, Quartiles) ->
    Tree = Quartiles#work_quartiles.tree,
    Tree2 = update_work_tree(ActorId, PrevShare, UpdatedShare, Tree),
    %Tree3 = gb_sets:balance(Tree2),
    Quartiles#work_quartiles{ tree = Tree2 }.

update_work_tree(ActorId, PrevShare, UpdatedShare, Tree) when PrevShare =:= 0 ->
    % actor added
    %gb_sets:add({UpdatedShare, ActorId}, Tree);
    aequitas_quartiles:add({UpdatedShare, ActorId}, Tree);
update_work_tree(ActorId, PrevShare, UpdatedShare, Tree) when UpdatedShare =:= 0 ->
    % actor removed
    %gb_sets:delete({PrevShare, ActorId}, Tree);
    aequitas_quartiles:delete({PrevShare, ActorId}, Tree);
update_work_tree(ActorId, PrevShare, UpdatedShare, Tree) ->
    % actor updated
    %Tree2 = gb_sets:delete({PrevShare, ActorId}, Tree),
    Tree2 = aequitas_quartiles:delete({PrevShare, ActorId}, Tree),
    %gb_sets:add({UpdatedShare, ActorId}, Tree2).
    aequitas_quartiles:add({UpdatedShare, ActorId}, Tree2).

zscore(ActorId, State) ->
    % Standard score / Z-score:
    % https://en.wikipedia.org/wiki/Standard_score
    WorkStats = State#state.work_stats,
    case WorkStats#work_stats.stddev of
        0.0 ->
            0.0;
        StdDev ->
            WorkShare = maps:get(ActorId, State#state.work_shares, 0),
            (WorkShare - WorkStats#work_stats.mean) / StdDev
    end.
