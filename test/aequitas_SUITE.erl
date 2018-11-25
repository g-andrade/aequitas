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

-module(aequitas_SUITE).
-compile(export_all).

-include_lib("eunit/include/eunit.hrl").

%% ------------------------------------------------------------------
%% Enumeration
%% ------------------------------------------------------------------

all() ->
    individual_test_cases()
    ++ [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [{Group, [parallel], group_test_cases()}
     || Group <- ['10actors_20mean_10dev_1.5iqr_1sync',
                  '10actors_20mean_10dev_1.5iqr_0sync',
                  '100actors_200mean_100dev_1.5iqr_1sync',
                  '100actors_200mean_100dev_1.5iqr_0sync',
                  '1000actors_20mean_10dev_2.0iqr_1sync',
                  '1000actors_20mean_10dev_2.0iqr_0sync',
                  '10000actors_3mean_0dev_0.5iqr_1sync',
                  '10000actors_3mean_0dev_0.5iqr_0sync',
                  '10actors_100mean_20dev_3.0iqr_1sync',
                  '10actors_100mean_20dev_3.0iqr_0sync',
                  '100actors_10mean_0dev_10.0iqr_1sync',
                  '100actors_10mean_0dev_10.0iqr_0sync'
                 ]].

individual_test_cases() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    [Name || {Name, 1} <- Exports, lists:suffix("_test", atom_to_list(Name))].

group_test_cases() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    [Name || {Name, 1} <- Exports, lists:suffix("_grouptest", atom_to_list(Name))].

%% ------------------------------------------------------------------
%% Initialization
%% ------------------------------------------------------------------

init_per_group(Group, Config) ->
    {ok, _} = application:ensure_all_started(aequitas),
    [{group, Group}]
    ++ group_params(Group)
    ++ Config.

end_per_group(_Group, Config) ->
    ok = application:stop(aequitas),
    Config.

group_params(Group) ->
    Str = atom_to_list(Group),
    Tokens = string:tokens(Str, [$_]),
    [{nr_of_actors, match_suffixed_param(Tokens, "actors")},
     {nr_of_requests_mean, match_suffixed_param(Tokens, "mean")},
     {nr_of_requests_stddev, match_suffixed_param(Tokens, "dev")},
     {iqr_factor, match_suffixed_param(Tokens, "iqr")},
     {is_sync, match_suffixed_param(Tokens, "sync") =:= 1}
    ].

match_suffixed_param([H|T], Suffix) ->
    case lists:suffix(Suffix, H) of
        true ->
            ParamStr = lists:sublist(H, length(H) - length(Suffix)),
            try list_to_float(ParamStr) of
                Float ->
                    Float
            catch
                error:badarg ->
                    list_to_integer(ParamStr)
            end;
        false ->
            match_suffixed_param(T, Suffix)
    end.

init_per_testcase(TestCase, Config) ->
    {ok, _} = application:ensure_all_started(aequitas),
    case proplists:get_value(group, Config) of
        undefined ->
            Config;
        Group ->
            Category = {Group, TestCase},
            IqrFactor = proplists:get_value(iqr_factor, Config),
            ok = aequitas:start(
                   Category, [{max_window_size, infinity},
                              {max_window_duration, {minutes,10}},
                              {min_actor_count, 1},
                              {iqr_factor, IqrFactor}
                             ]),
            [{category, Category}
             | Config]
    end.

end_per_testcase(_TestCase, Config) ->
    case proplists:get_value(category, Config) of
        undefined ->
            Config;
        Category ->
            ok = aequitas:stop(Category),
            Config
    end.

%% ------------------------------------------------------------------
%% Definition
%% ------------------------------------------------------------------

static_configuration_test(_Config) ->
    % @see test/etc/sys.config
    NonAtomCategory = {static_configuration, non_atom_category},
    CategoryA = static_configuration_categoryA,
    CategoryB = static_configuration_categoryB,
    CategoryC = static_configuration_categoryC,
    ?assertEqual({ok, 999}, aequitas_category:get_current_setting(NonAtomCategory, max_window_size)),
    ?assertEqual({ok, 10}, aequitas_category:get_current_setting(CategoryA, max_window_size)),
    ?assertEqual({ok, 42}, aequitas_category:get_current_setting(CategoryB, max_window_size)),
    ?assertEqual({error, noproc}, aequitas_category:get_current_setting(CategoryC, max_window_size)).

static_configuration_update_test(_Config) ->
    NonAtomCategory = {static_configuration, non_atom_category},
    CategoryA = static_configuration_categoryA,
    CategoryB = static_configuration_categoryB,
    ?assertEqual({ok, 999}, aequitas_category:get_current_setting(NonAtomCategory, max_window_size)),
    ?assertEqual({ok, 10}, aequitas_category:get_current_setting(CategoryA, max_window_size)),
    ?assertEqual({ok, 42}, aequitas_category:get_current_setting(CategoryB, max_window_size)),

    % one category changed
    EnvBefore1 = full_apps_env(),
    {ok, SettingOptsPerCategory1} = application:get_env(aequitas, categories),
    SettingOptsPerCategory2 = SettingOptsPerCategory1#{ CategoryA := [{max_window_size,30}] },
    ok = application:set_env(aequitas, categories, SettingOptsPerCategory2),
    ok = application_controller:config_change(EnvBefore1),
    ?assertEqual({ok, 999}, aequitas_category:get_current_setting(NonAtomCategory, max_window_size)),
    ?assertEqual({ok, 30}, aequitas_category:get_current_setting(CategoryA, max_window_size)),
    ?assertEqual({ok, 42}, aequitas_category:get_current_setting(CategoryB, max_window_size)),

    % one category changed
    EnvBefore2 = full_apps_env(),
    SettingOptsPerCategory3 = SettingOptsPerCategory2#{ CategoryB := [{max_window_size,50}] },
    ok = application:set_env(aequitas, categories, SettingOptsPerCategory3),
    ok = application_controller:config_change(EnvBefore2),
    ?assertEqual({ok, 999}, aequitas_category:get_current_setting(NonAtomCategory, max_window_size)),
    ?assertEqual({ok, 30}, aequitas_category:get_current_setting(CategoryA, max_window_size)),
    ?assertEqual({ok, 50}, aequitas_category:get_current_setting(CategoryB, max_window_size)),

    % two categories changed
    EnvBefore3 = full_apps_env(),
    SettingOptsPerCategory4 = SettingOptsPerCategory1,
    ok = application:set_env(aequitas, categories, SettingOptsPerCategory4),
    ok = application_controller:config_change(EnvBefore3),
    ?assertEqual({ok, 999}, aequitas_category:get_current_setting(NonAtomCategory, max_window_size)),
    ?assertEqual({ok, 10}, aequitas_category:get_current_setting(CategoryA, max_window_size)),
    ?assertEqual({ok, 42}, aequitas_category:get_current_setting(CategoryB, max_window_size)),

    % no change at all
    EnvBefore4 = full_apps_env(),
    SettingOptsPerCategory5 = SettingOptsPerCategory4,
    ok = application:set_env(aequitas, categories, SettingOptsPerCategory5),
    ok = application_controller:config_change(EnvBefore4),
    ?assertEqual({ok, 999}, aequitas_category:get_current_setting(NonAtomCategory, max_window_size)),
    ?assertEqual({ok, 10}, aequitas_category:get_current_setting(CategoryA, max_window_size)),
    ?assertEqual({ok, 42}, aequitas_category:get_current_setting(CategoryB, max_window_size)).

static_configuration_override_test(_Config) ->
    Category = static_configuration_categoryA,
    ?assertEqual({ok, 10}, aequitas_category:get_current_setting(Category, max_window_size)),
    ok = aequitas:reconfigure(Category, [{max_window_size, 20}]),
    ?assertEqual({ok, 20}, aequitas_category:get_current_setting(Category, max_window_size)).

dynamic_reconfiguration_test(_Config) ->
    Category = dynamic_configuration_category,
    CategoryOpts = [{max_window_size, 23}],
    ok = aequitas:start(Category, CategoryOpts),
    ?assertEqual({ok, 23}, aequitas_category:get_current_setting(Category, max_window_size)),
    ok = aequitas:reconfigure(Category, [{max_window_size, 46}]),
    ?assertEqual({ok, 46}, aequitas_category:get_current_setting(Category, max_window_size)).

rate_limited_acceptances_test(_Config) ->
    Category = rate_limited_acceptances_test,
    ExpectedRate = 200,
    CategoryOpts =
        [{min_actor_count, 1 bsl 128}, % disable outlier detection entirely
         {max_collective_rate, ExpectedRate}
        ],
    ok = aequitas:start(Category, CategoryOpts),

    Self = self(),
    DurationSeconds = 3,
    Duration = timer:seconds(DurationSeconds),
    NrOfActors = 100,
    WorkerPid = spawn(fun () -> rate_limit_test_worker(Self, Category, NrOfActors, Duration) end),
    WorkerMon = monitor(process, WorkerPid),
    receive
        {WorkerPid, CountPerStatus} ->
            {ok, AcceptedCount} = dict:find(accepted, CountPerStatus),
            AcceptedRate = AcceptedCount / DurationSeconds,
            Ratio = AcceptedRate / ExpectedRate,
            ct:pal("AcceptedRate: ~p", [AcceptedRate]),
            ct:pal("ExpectedRate: ~p", [ExpectedRate]),
            ct:pal("Ratio: ~p", [Ratio]),
            ?assert(Ratio >= 0.75),
            ?assert(Ratio =< 1.25),
            ok = aequitas:stop(Category);
        {'DOWN', WorkerMon, process, _Pid, Reason} ->
            error({worker_died, Reason})
    end.

rate_unlimited_acceptances_test(_Config) ->
    Category = rate_unlimited_acceptances_test,
    CategoryOpts =
        [{min_actor_count, 1 bsl 128}, % disable outlier detection entirely
         {max_collective_rate, infinity}
        ],
    ok = aequitas:start(Category, CategoryOpts),

    Self = self(),
    DurationSeconds = 3,
    Duration = timer:seconds(DurationSeconds),
    NrOfActors = 100,
    WorkerPid = spawn(fun () -> rate_limit_test_worker(Self, Category, NrOfActors, Duration) end),
    WorkerMon = monitor(process, WorkerPid),
    receive
        {WorkerPid, CountPerStatus} ->
            ?assertEqual(error, dict:find({rejected,outlier}, CountPerStatus)),
            ?assertEqual(error, dict:find({rejected,rate_limited}, CountPerStatus)),
            ok = aequitas:stop(Category);
        {'DOWN', WorkerMon, process, _Pid, Reason} ->
            error({worker_died, Reason})
    end.

correct_iqr_enforcement_grouptest(Config) ->
    NrOfActors = proplists:get_value(nr_of_actors, Config),
    NrOfRequestsMean = proplists:get_value(nr_of_requests_mean, Config),
    NrOfRequestsStdDev = proplists:get_value(nr_of_requests_stddev, Config),
    ActorRequests =
        lists:foldl(
          fun (Actor, Acc) ->
                  NrOfRequests = max(0, round(NrOfRequestsMean + (NrOfRequestsStdDev * rand:normal()))),
                  [Actor || _ <- lists:seq(1, NrOfRequests)]
                  ++ Acc
          end,
          [], lists:seq(1, NrOfActors)),
    ShuffledActorRequests =
        lists_shuffle(ActorRequests),
    correct_iqr_enforcement_grouptest(ShuffledActorRequests, Config, #{}).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

full_apps_env() ->
    All = [App || {App, _Desc, _Vsn} <- application:which_applications()],
    [{App, application:get_all_env(App)} || App <- All].

rate_limit_test_worker(Parent, Category, NrOfActors, Duration) ->
    erlang:send_after(Duration, self(), finished),
    rate_limit_test_worker_loop(Parent, Category, NrOfActors, dict:new()).

rate_limit_test_worker_loop(Parent, Category, NrOfActors, Acc) ->
    ActorId = rand:uniform(NrOfActors),
    {Tag, Mon} = aequitas:async_ask(Category, ActorId),
    receive
        finished ->
            Parent ! {self(), Acc};
        {Tag, Result} ->
            demonitor(Mon, [flush]),
            UpdatedAcc = dict:update_counter(Result, +1, Acc),
            rate_limit_test_worker_loop(Parent, Category, NrOfActors, UpdatedAcc);
        {'DOWN', Mon, process, _Pid, Reason} ->
            exit({category_died, Reason})
    end.

correct_iqr_enforcement_grouptest([Actor | NextActors], Config, WorkShares) ->
    Category = proplists:get_value(category, Config),
    AskOpts = [return_stats],

    {AskResult, Stats} = ask(Category, Actor, AskOpts, Config),
    IqrFactor = proplists:get_value(iqr_factor, Config),
    ExpectedAskResult = expected_ask_result(Actor, IqrFactor, WorkShares, Stats),
    ?assertEqual(ExpectedAskResult, AskResult),

    UpdatedWorkShares =
        case AskResult of
            accepted ->
                WorkShare = maps:get(Actor, WorkShares, 0),
                WorkShares#{ Actor => WorkShare + 1 };
            {rejected,_Reason} ->
                WorkShares
        end,
    correct_iqr_enforcement_grouptest(NextActors, Config, UpdatedWorkShares);
correct_iqr_enforcement_grouptest([], _Config, _WorkShares) ->
    ok.

ask(Category, Actor, AskOpts, Config) ->
    case proplists:get_value(is_sync, Config) of
        true ->
            aequitas:ask(Category, Actor, AskOpts);
        false ->
            {Tag, Monitor} = aequitas:async_ask(Category, Actor, AskOpts),
            receive
                {Tag, Result} ->
                    demonitor(Monitor, [flush]),
                    Result;
                {'DOWN', Monitor, process, _Pid, Reason} ->
                    error({category_died, Reason})
            end
    end.

expected_ask_result(Actor, IqrFactor, WorkShares, Stats) ->
    case Stats of
        #{ q3 := Q3, iqr := IQR } ->
           WorkShare = maps:get(Actor, WorkShares, 0),
           WorkLimit = Q3 + (IQR * IqrFactor),
           case WorkShare > WorkLimit of
               true -> {rejected,outlier};
               false -> accepted
           end;
        #{} ->
            accepted
    end.

lists_shuffle(List) ->
    WithTags = [{V, rand:uniform()} || V <- List],
    Sorted = lists:keysort(2, WithTags),
    [V || {V, _Tag} <- Sorted].
