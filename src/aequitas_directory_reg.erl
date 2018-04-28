-module(aequitas_directory_reg).
-behaviour(gen_statem).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export(
   [start_link/2,
    start/2
   ]).

%% ------------------------------------------------------------------
%% gen_statem Function Exports
%% ------------------------------------------------------------------

-export([callback_mode/0]).
-export([init/1]).
-export([handle_event/4]).
-export([terminate/3]).

%% ------------------------------------------------------------------
%% Macro Definitions
%% ------------------------------------------------------------------

-define(CB_MODULE, ?MODULE).

%% ------------------------------------------------------------------
%% Record and Type Definitions
%% ------------------------------------------------------------------

-record(sdata, {
          name :: term(),
          mon :: reference()
         }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Name, Pid) ->
    gen_statem:start_link(?CB_MODULE, [Name, Pid], []).

start(Name, Pid) ->
    aequitas_directory_reg_sup:start_child([Name, Pid]).

%% ------------------------------------------------------------------
%% gen_statem Function Definitions
%% ------------------------------------------------------------------

callback_mode() -> 
    handle_event_function.

init([Name, Pid]) ->
    process_flag(trap_exit, true), % always call terminate/3
    case aequitas_directory:insert_new_entry(Name, Pid) of
        true ->
            Mon = monitor(process, Pid),
            SData = #sdata{ name = Name,
                            mon = Mon 
                          },
            {ok, monitoring, SData};
        false ->
            {stop, {shutdown, already_started}}
    end.

handle_event(info, {'DOWN', Ref, process, _Pid, _Reason}, _State, SData)
  when Ref =:= SData#sdata.mon ->
    stop;
handle_event(EventType, EventContent, _State, _SData) ->
    {stop, {unexpected_event, EventType, EventContent}}.

terminate(_Reason, _State, SData) ->
    aequitas_directory:delete_entry(SData#sdata.name),
    ok.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
