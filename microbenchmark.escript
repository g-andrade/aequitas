-module(microbenchmark).
-mode(compile).

-export([main/1]).

main([]) ->
    Category = microbenchmarking,
    NrOfWorkers = 100,
    NrOfCalls = 2000000,
    {ok, _} = application:ensure_all_started(aequitas),
    {ok, _} = application:ensure_all_started(sasl),
    do_it(Category, NrOfWorkers, NrOfCalls).

do_it(Category, NrOfWorkers, NrOfCalls) ->
    NrOfCallsPerWorker = NrOfCalls div NrOfWorkers,
    Parent = self(),
    Pids = [spawn(fun () -> run_worker(Category, Nr, Parent, NrOfCallsPerWorker) end)
            || Nr <- lists:seq(1, NrOfWorkers)],
    WithMonitors = [{Pid, monitor(process, Pid)} || Pid <- Pids],
    io:format("running benchmarks... (~p calls using ~p workers)",
              [NrOfCalls, NrOfWorkers]),
    wait_for_workers(WithMonitors, [], []).

wait_for_workers([], ResultAcc, SecondsToGenerateAcc) ->
    UniqueAequitasResults = lists:usort( lists:flatten([maps:keys(M) || M <- ResultAcc]) ),
    lists:foreach(
      fun (AequitasResult) ->
              Counts = [maps:get(AequitasResult, M, 0) || M <- ResultAcc],
              TotalCount = trunc(lists:sum(Counts)),
              io:format("achieved an average of ~p '~p' results per second~n",
                        [TotalCount, AequitasResult])
      end,
      UniqueAequitasResults),
    StatsOfSecondsToGenerateStats =
        #{ min => lists:min(SecondsToGenerateAcc),
           max => lists:max(SecondsToGenerateAcc),
           avg => lists:sum(SecondsToGenerateAcc) / length(SecondsToGenerateAcc)
         },
    io:format("stats of stats' seconds_to_generate: ~p~n", [StatsOfSecondsToGenerateStats]),
    erlang:halt();
wait_for_workers(WithMonitors, ResultAcc, SecondsToGenerateAcc) ->
    receive
        {worker_result, Pid, Result, WorkerSecondsToGenerateAcc} ->
            {value, {Pid, Monitor}, UpdatedWithMonitors} = lists:keytake(Pid, 1, WithMonitors),
            demonitor(Monitor, [flush]),
            UpdatedResultsAcc = [Result | ResultAcc],
            UpdatedSecondsToGenerateAcc = WorkerSecondsToGenerateAcc ++ SecondsToGenerateAcc,
            wait_for_workers(UpdatedWithMonitors, UpdatedResultsAcc, UpdatedSecondsToGenerateAcc);
        {'DOWN', _Ref, process, _Pid, Reason} ->
            error(Reason)
    end.

run_worker(Category, Nr, Parent, NrOfCalls) ->
    run_worker_loop(Category, Nr, Parent, NrOfCalls,
                    erlang:monotonic_time(milli_seconds), 0, #{}, []).

run_worker_loop(_Category, _Nr, Parent, NrOfCalls, StartTs,
                Count, CountPerResult, SecondsToGenerateAcc) when Count =:= NrOfCalls ->
    EndTs = erlang:monotonic_time(milli_seconds),
    TimeElapsed = EndTs - StartTs,
    AdjustedCountPerResult =
        maps:map(
          fun (_Result, Count) ->
                  (Count / (TimeElapsed / 1000))
          end,
          CountPerResult),
    Parent ! {worker_result, self(), AdjustedCountPerResult, SecondsToGenerateAcc};
run_worker_loop(Category, Nr, Parent, NrOfCalls, StartTs, Count, CountPerResult, SecondsToGenerateAcc) ->
    {Result, Stats} = aequitas:ask(Category, Nr, [return_stats]),
    UpdatedCountPerResult = maps_increment(Result, +1, CountPerResult),
    SecondsToGenerateStats = maps:get(seconds_to_generate, Stats),
    UpdatedSecondsToGenerateAcc = [SecondsToGenerateStats | SecondsToGenerateAcc],
    run_worker_loop(Category, Nr, Parent, NrOfCalls, StartTs, Count + 1,
                    UpdatedCountPerResult, UpdatedSecondsToGenerateAcc).

maps_increment(Key, Incr, Map) ->
    maps:update_with(
      Key,
      fun (Value) -> Value + Incr end,
      Incr, Map).
