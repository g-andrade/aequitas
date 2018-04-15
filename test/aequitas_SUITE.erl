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
    [{group, GroupName} || {GroupName, _Options, _TestCases} <- groups()].

groups() ->
    [{Group, [parallel], test_cases()}
     || Group <- ['10actors_20mean_10dev',
                  '100actors_20mean_10dev',
                  '1000actors_20mean_10dev',
                  '10actors_100mean_20dev',
                  '100actors_10mean_0dev',
                  '100actors_0mean_0dev']].

test_cases() ->
    ModuleInfo = ?MODULE:module_info(),
    {exports, Exports} = lists:keyfind(exports, 1, ModuleInfo),
    [Name || {Name, 1} <- Exports, lists:suffix("_test", atom_to_list(Name))].

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
     {max_zscore_range, {-5, 5}}
    ].

match_suffixed_param([H|T], Suffix) ->
    case lists:suffix(Suffix, H) of
        true ->
            ParamStr = lists:sublist(H, length(H) - length(Suffix)),
            list_to_integer(ParamStr);
        false ->
            match_suffixed_param(T, Suffix)
    end.

init_per_testcase(TestCase, Config) ->
    Group = proplists:get_value(group, Config),
    Category = list_to_atom(
                 atom_to_list(Group)
                 ++ "."
                 ++ atom_to_list(TestCase)),

    ok = aequitas:configure(Category, [{max_window_size, infinity},
                                       {max_window_duration, infinity}]),
    [{category, Category}
     | Config].

%% ------------------------------------------------------------------
%% Definition
%% ------------------------------------------------------------------

correct_zscore_enforcement_test(Config) ->
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
    correct_zscore_enforcement_test(ShuffledActorRequests, Config, #{}).

%% ------------------------------------------------------------------
%% Internal
%% ------------------------------------------------------------------

correct_zscore_enforcement_test([Actor | NextActors], Config, AcceptancesPerActor) ->
    Category = proplists:get_value(category, Config),
    {MinMaxZScore, MaxMaxZScore} = proplists:get_value(max_zscore_range, Config),
    MaxZScore = MinMaxZScore + (rand:uniform(MaxMaxZScore - MinMaxZScore) - 1),
    AskOpts = [{max_zscore, MaxZScore}],

    ExpectedAskResult = expected_ask_result(Actor, MaxZScore, AcceptancesPerActor),
    AskResult = aequitas:ask(Category, Actor, AskOpts),
    ?assertEqual(ExpectedAskResult, AskResult),

    UpdatedAcceptancesPerActor =
        case AskResult of
            accepted ->
                WorkShare = maps:get(Actor, AcceptancesPerActor, 0),
                AcceptancesPerActor#{ Actor => WorkShare + 1 };
            rejected ->
                AcceptancesPerActor
        end,
    correct_zscore_enforcement_test(NextActors, Config, UpdatedAcceptancesPerActor);
correct_zscore_enforcement_test([], _Config, _AcceptancesPerActor) ->
    ok.

expected_ask_result(Actor, MaxZScore, AcceptancesPerActor) ->
    ZScore = zscore(Actor, AcceptancesPerActor),
    case ZScore > MaxZScore of
        true -> rejected;
        false -> accepted
    end.

zscore(Actor, AcceptancesPerActor) ->
    {Mean, StdDeviation} = work_stats(AcceptancesPerActor),
    case StdDeviation == 0 of
        true -> 0;
        false ->
            Acceptances = maps:get(Actor, AcceptancesPerActor, 0),
            (Acceptances - Mean) / StdDeviation
    end.

work_stats(AcceptancesPerActor) when map_size(AcceptancesPerActor) =:= 0 ->
    {0, 0};
work_stats(AcceptancesPerActor) ->
    {Sum, SquaredSum} =
        maps:fold(
          fun (_Actor, Acceptances, {SumAcc, SquaredSumAcc}) ->
                  {SumAcc + Acceptances,
                   SquaredSumAcc + (Acceptances * Acceptances)}
          end,
          {0, 0}, AcceptancesPerActor),

    N = map_size(AcceptancesPerActor),
    Mean = Sum / N,
    Variance = (SquaredSum / N) - (Mean * Mean),
    StdDeviation = math:sqrt(Variance),
    {Mean, StdDeviation}.

lists_shuffle(List) ->
    WithTags = [{V, rand:uniform()} || V <- List],
    Sorted = lists:keysort(2, WithTags),
    [V || {V, _Tag} <- Sorted].
