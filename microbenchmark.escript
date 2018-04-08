-module(microbenchmark).
-mode(compile).

-export([main/1]).

main([]) ->
    Group = microbenchmarking,
    NrOfWorkers = 100,
    NrOfCalls = 2000000,
    {ok, _} = application:ensure_all_started(aequitas),
    {ok, _} = application:ensure_all_started(sasl),
    do_it(Group, NrOfWorkers, NrOfCalls).

do_it(Group, NrOfWorkers, NrOfCalls) ->
    NrOfCallsPerWorker = NrOfCalls div NrOfWorkers,
    Parent = self(),
    Pids = [spawn(fun () -> run_worker(Group, Nr, Parent, NrOfCallsPerWorker) end)
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

run_worker(Group, Nr, Parent, NrOfCalls) ->
    run_worker_loop(Group, Nr, Parent, NrOfCalls, erlang:monotonic_time(), 0).

run_worker_loop(_Group, _Nr, Parent, NrOfCalls, StartTs, Count) when Count =:= NrOfCalls ->
    EndTs = erlang:monotonic_time(),
    TimeElapsed = EndTs - StartTs,
    PerSecond = (Count * erlang:convert_time_unit(1, second, native)) / TimeElapsed,
    Parent ! {worker_result, self(), PerSecond};
run_worker_loop(Group, Nr, Parent, NrOfCalls, StartTs, Count) ->
    _ = aequitas:ask(Group, Nr),
    run_worker_loop(Group, Nr, Parent, NrOfCalls, StartTs, Count + 1).
