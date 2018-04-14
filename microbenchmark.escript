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
    wait_for_workers(WithMonitors, []).

wait_for_workers([], ResultAcc) ->
    TotalCount = trunc( lists:sum(ResultAcc) ),
    io:format("achieved an average of ~p calls per second", [TotalCount]),
    erlang:halt();
wait_for_workers(WithMonitors, ResultAcc) ->
    receive
        {worker_result, Pid, Result} ->
            {value, {Pid, Monitor}, UpdatedWithMonitors} = lists:keytake(Pid, 1, WithMonitors),
            demonitor(Monitor, [flush]),
            UpdatedResultsAcc = [Result | ResultAcc],
            wait_for_workers(UpdatedWithMonitors, UpdatedResultsAcc);
        {'DOWN', _Ref, process, _Pid, Reason} ->
            error(Reason)
    end.

run_worker(Category, Nr, Parent, NrOfCalls) ->
    run_worker_loop(Category, Nr, Parent, NrOfCalls, erlang:monotonic_time(milli_seconds), 0).

run_worker_loop(_Category, _Nr, Parent, NrOfCalls, StartTs, Count) when Count =:= NrOfCalls ->
    EndTs = erlang:monotonic_time(milli_seconds),
    TimeElapsed = EndTs - StartTs,
    PerSecond = (Count / (TimeElapsed / 1000)),
    Parent ! {worker_result, self(), PerSecond};
run_worker_loop(Category, Nr, Parent, NrOfCalls, StartTs, Count) ->
    _ = aequitas:ask(Category, Nr),
    run_worker_loop(Category, Nr, Parent, NrOfCalls, StartTs, Count + 1).
