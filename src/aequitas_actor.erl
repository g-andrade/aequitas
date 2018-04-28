-module(aequitas_actor).
-behaviour(gen_statem).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/1,
    upsert_actor_process/1 % FIXME temporary
   ]).

%% ------------------------------------------------------------------
%% gen_statem Function Exports
%% ------------------------------------------------------------------

-export([callback_mode/0]).
-export([init/1]).
%% states
-export([doing_something/3]).
%%
%-export([terminate/3]).
%-export([code_change/4]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(sdata, {
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name) ->
    gen_statem:start_link(?CB_MODULE, [Name], []).

ask(Name) ->
    Pid = upsert_actor_process(Name),

%% ------------------------------------------------------------------
%% gen_statem Function Definitions
%% ------------------------------------------------------------------

callback_mode() -> 
    state_functions.

init([Name]) ->
    case aequitas_directory:register(Name) of
        ok ->
            {ok, doing_something, #sdata{}};
        {error, already_started} ->
            {stop, {shutdown, already_started}}
    end.

doing_something(EventType, EventContent, _SData) ->
    {stop, {unexpected_event, EventType, EventContent}}.

%terminate(_Reason, _State, _Data) ->
%    ok.

%code_change(_OldVsn, State, StateData, _Extra) ->
%    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

upsert_actor_process(Name) ->
    case aequitas_directory:whereis(Name) of
        undefined ->
            insert_actor_process(Name);
        Pid ->
            Pid
    end.

insert_actor_process(Name) ->
    case aequitas_actor_sup:start_child([Name]) of
        {ok, Pid} ->
            Pid;
        {error, {shutdown, already_started}} ->
            upsert_actor_process(Name)
    end.
