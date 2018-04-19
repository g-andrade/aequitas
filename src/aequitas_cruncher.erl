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

%% @private
-module(aequitas_cruncher).

% https://gist.github.com/marcelog/97708058cd17f86326c82970a7f81d40#file-simpleproc-erl

%%-------------------------------------------------------------------
%% API Function Exports
%%-------------------------------------------------------------------

-export(
   [start_link/1,
    start/1,
    generate_work_stats/2
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
%% Record and Type Definitions
%%-------------------------------------------------------------------

-record(state, {
          category_pid :: pid(),
          category_mon :: reference()
         }).
-type state() :: #state{}.

-type work_stats() ::
        #{ nr_of_samples => non_neg_integer(),
           q1 => number(),
           q2 => number(),
           q3 => number(),
           iqr => number()
         }.
-export_type([work_stats/0]).

%%-------------------------------------------------------------------
%% API Function Definitions
%%-------------------------------------------------------------------

-spec start_link(pid()) -> {ok, pid()}.
start_link(CategoryPid) ->
    proc_lib:start_link(?MODULE, init, [{self(), [CategoryPid]}]).

-spec start(pid()) -> {ok, pid()}.
start(CategoryPid) ->
    aequitas_cruncher_sup:start_child([CategoryPid]).

-spec generate_work_stats(pid(), #{ term() => pos_integer() }) -> ok.
generate_work_stats(CruncherPid, WorkShares) ->
    CruncherPid ! {generate_work_stats, WorkShares},
    ok.

%%-------------------------------------------------------------------
%% OTP Function Definitions
%%-------------------------------------------------------------------

-spec init({pid(), [pid(), ...]}) -> no_return().
init({Parent, [CategoryPid]}) ->
    proc_lib:init_ack(Parent, {ok, self()}),
    Debug = sys:debug_options([]),
    State =
        #state{
           category_pid = CategoryPid,
           category_mon = monitor(process, CategoryPid)
          },
    loop(Parent, Debug, State).

-spec write_debug(io:device(), term(), term()) -> ok.
write_debug(Dev, Event, Name) ->
    % called by sys:handle_debug().
    io:format(Dev, "~p event = ~p~n", [Name, Event]).

-spec system_continue(pid(), [sys:debug_opt()], state()) -> no_return().
system_continue(Parent, Debug, State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_continue-3
    loop(Parent, Debug, State).

-spec system_terminate(term(), pid(), [sys:debug_opt()], state()) -> no_return().
system_terminate(Reason, _Parent, _Debug, _State) ->
    % http://www.erlang.org/doc/man/sys.html#Mod:system_terminate-4
    exit(Reason).

-spec system_code_change(state(), ?MODULE, term(), term()) -> {ok, state()}.
%% http://www.erlang.org/doc/man/sys.html#Mod:system_code_change-4
system_code_change(State, _Module, _OldVsn, _Extra) ->
    {ok, State}.

%%-------------------------------------------------------------------
%% Internal Functions Definitions - Execution Loop
%%-------------------------------------------------------------------

loop(Parent, Debug, State) ->
    receive
        {system, From, Request} ->
            sys:handle_system_msg(Request, From, Parent, ?MODULE, Debug, State);
        Msg ->
            UpdatedDebug = sys:handle_debug(Debug, fun ?MODULE:write_debug/3, ?MODULE, {in, Msg}),
            UpdatedState = handle_nonsystem_msg(Msg, State),
            loop(Parent, UpdatedDebug, UpdatedState)
    after
        5000 ->
            hibernate(Parent, Debug, State)
    end.

handle_nonsystem_msg({generate_work_stats, WorkShares}, State) ->
    WorkStats = crunch_work_stats(WorkShares),
    aequitas_category:report_work_stats(State#state.category_pid, WorkStats),
    State;
handle_nonsystem_msg({'DOWN', Ref, process, _Pid, _Reason}, State)
  when Ref =:= State#state.category_mon ->
    exit(normal);
handle_nonsystem_msg(Msg, _State) ->
    error({unexpected_msg, Msg}).

hibernate(Parent, Debug, State) ->
    proc_lib:hibernate(?MODULE, system_continue, [Parent, Debug, State]).

%%-------------------------------------------------------------------
%% Internal Functions Definitions - Requests
%%-------------------------------------------------------------------

crunch_work_stats(WorkShares) ->
    NrOfSamples = map_size(WorkShares),
    case NrOfSamples < 3 of
        true ->
            % not enough samples
            #{ nr_of_samples => NrOfSamples };
        false ->
            Samples = maps:values(WorkShares),
            SortedSamples = lists:sort(Samples),
            {Q2, LowerHalf, UpperHalf} = median_split(SortedSamples),
            {Q1, _, _} = median_split(LowerHalf),
            {Q3, _, _} = median_split(UpperHalf),
            #{ nr_of_samples => NrOfSamples,
               q1 => Q1,
               q2 => Q2,
               q3 => Q3,
               iqr => Q3 - Q1
             }
    end.

median_split([Median]) ->
    {Median, [], []};
median_split(List) ->
    Len = length(List),
    HalfLen = Len div 2,
    case Len rem HalfLen of
        0 ->
            {Left, Right} = lists:split(HalfLen, List),
            Median = (lists:last(Left) + hd(Right)) / 2,
            {Median, Left, Right};
        1 ->
            {Left, [Median | Right]} = lists:split(HalfLen, List),
            {Median, Left, Right}
    end.
