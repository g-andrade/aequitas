%% Copyright (c) 2018-2021 Guilherme Andrade
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
-module(aequitas_proc_reg).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/0,
    register/2,
    whereis/1
   ]).

-ignore_xref(
   [start_link/0
   ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export(
   [init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3
   ]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).
-define(SERVER, ?MODULE).
-define(TABLE, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(state, {
          monitors :: #{ reference() => term() }
         }).
-type state() :: #state{}.

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

-spec start_link() -> {ok, pid()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?CB_MODULE, [], []).

-spec register(term(), pid()) -> ok | {error, {already_registered, pid()}}.
register(Name, Pid) ->
    gen_server:call(?SERVER, {register, Name, Pid}, infinity).

-spec whereis(term()) -> pid() | undefined.
whereis(Name) ->
    case ets:lookup(?TABLE, Name) of
        [{_, Pid}] -> Pid;
        _ -> undefined
    end.

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-spec init([]) -> {ok, state()}.
init([]) ->
    EtsOpts = [named_table, protected, {read_concurrency,true}],
    _ = ets:new(?TABLE, EtsOpts),
    {ok, #state{ monitors = #{} }}.

-spec handle_call(term(), {pid(),reference()}, state())
        -> {reply, Reply, state()} |
           {stop, unexpected_call, state()}
    when Reply :: ok | {error, {already_registered,pid()}}.
handle_call({register, Name, Pid}, _From, State) ->
    case ets:lookup(?TABLE, Name) of
        [{_, ExistingPid}] ->
            {reply, {error, {already_registered, ExistingPid}}, State};
        [] ->
            ets:insert(?TABLE, {Name,Pid}),
            NewMonitor = monitor(process, Pid),
            Monitors = State#state.monitors,
            UpdatedMonitors = Monitors#{ NewMonitor => Name },
            UpdatedState = State#state{ monitors = UpdatedMonitors },
            {reply, ok, UpdatedState}
    end;
handle_call(_Call, _From, State) ->
    {stop, unexpected_call, State}.

-spec handle_cast(term(), state()) -> {stop, unexpected_cast, state()}.
handle_cast(_Cast, State) ->
    {stop, unexpected_cast, State}.

-spec handle_info(term(), state())
        -> {noreply, state()} |
           {stop, unexpected_info, state()}.
handle_info({'DOWN', Ref, process, _Pid, _Reason}, State) ->
    Monitors = State#state.monitors,
    {Name, UpdatedMonitors} = maps_take(Ref, Monitors),
    [_] = ets:take(?TABLE, Name),
    UpdatedState = State#state{ monitors = UpdatedMonitors },
    {noreply, UpdatedState};
handle_info(_Info, State) ->
    {stop, unexpected_info, State}.

-spec terminate(term(), state()) -> ok.
terminate(_Reason, _State) ->
    ok.

-spec code_change(term(), state(), term()) -> {ok, state()}.
code_change(_OldVsn, #state{} = State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maps_take(Key, Map) ->
    % OTP 18 doesn't include maps:take/2
    case maps:find(Key, Map) of
        {ok, Value} ->
            UpdatedMap = maps:remove(Key, Map),
            {Value, UpdatedMap};
        error ->
            error
    end.
